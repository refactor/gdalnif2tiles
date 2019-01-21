#include "mylog.h"

#include <erl_nif.h>
#include <gdal.h>
#include <cpl_conv.h>

#include <ogr_srs_api.h>

#include "utils.h"

void MyGDALErrorHandler(CPLErr eErrClass, int errNo, const char *msg) {
    if (eErrClass <= CE_Warning) {
        WARN_LOG("errno: %d, %s", errNo, msg);
    } else {
        ERR_LOG("errno: %d, %s", errNo, msg);
    }
}

static ErlNifResourceType* gdalDatasetResType;

typedef struct MyGDALDataset {
    GDALDatasetH handle;
    OGRSpatialReferenceH srs;
    int srid;
    int rasterCount;
    int rasterWidth, rasterHeight;
    double originX, originY;
    double pixelWidth, pixelHeight;
    double minBoundX, maxBoundX;
    double minBoundY, maxBoundY;
    double minBoundZ, maxBoundZ;
} MyGDALDataset;

static ERL_NIF_TERM ATOM_OK;

void dataset_dtor(ErlNifEnv* env, void* obj) {
    MyGDALDataset *pGDALDataset = (MyGDALDataset*)obj;
    LOGA("pGDALDataset -> %p, handle -> %p", pGDALDataset, pGDALDataset->handle);
    if (pGDALDataset) {
        if (pGDALDataset->handle != NULL) {
            GDALClose(pGDALDataset->handle);
            pGDALDataset->handle = NULL;
        }
        if (pGDALDataset->srs != NULL) {
            OSRDestroySpatialReference(pGDALDataset->srs);
            pGDALDataset->srs = NULL;
        }
    }
}

static ERL_NIF_TERM open_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char filename[128] = {};
    if (!enif_get_string(env, argv[0], filename, sizeof(filename), ERL_NIF_LATIN1)) {
        return enif_raise_exception(env,
            enif_make_string(env, "No input file was specified", ERL_NIF_LATIN1));
    }
    LOGA("opening...");
    GDALDatasetH hDataset = GDALOpen(filename, GA_ReadOnly);
    LOGA("hDataset -> %p", hDataset);
    if (hDataset == NULL) {
        return enif_raise_exception(env,
            enif_make_string(env, "It is not possible to open the input file", ERL_NIF_LATIN1));
    }
    MyGDALDataset *pGDALDataset = enif_alloc_resource(gdalDatasetResType, sizeof(*pGDALDataset));
    pGDALDataset->handle = hDataset;
    pGDALDataset->rasterWidth  = GDALGetRasterXSize(hDataset);
    pGDALDataset->rasterHeight = GDALGetRasterYSize(hDataset);
    pGDALDataset->rasterCount  = GDALGetRasterCount(hDataset);
    double adfGeoTransform[6];
    if (GDALGetGeoTransform(hDataset, adfGeoTransform) == CE_None) {
        if (adfGeoTransform[2] != 0.0 || adfGeoTransform[4] != 0.0) {
            return enif_raise_exception(env, 
                    enif_make_tuple2(env, enif_make_atom(env, "not_support"),
                        enif_make_string(env, "Georeference of the raster contains rotation or skew", ERL_NIF_LATIN1)));
        }
    }
    pGDALDataset->originX = adfGeoTransform[0];
    pGDALDataset->originY = adfGeoTransform[3];
    pGDALDataset->pixelWidth = adfGeoTransform[1];
    pGDALDataset->pixelHeight = adfGeoTransform[5];
    LOGA("coeff[2]: %f, coeff[4]: %f", adfGeoTransform[2],adfGeoTransform[4]);
    
    const char* proj = GDALGetProjectionRef(hDataset);
    OGRSpatialReferenceH fileSRS = OSRNewSpatialReference(NULL);
    if (proj == NULL || OGRERR_NONE != OSRSetFromUserInput(fileSRS, proj)) {
        OSRDestroySpatialReference(fileSRS);
        return enif_raise_exception(env,
                enif_make_string(env, "NO spatial reference found", ERL_NIF_LATIN1));
    }
    pGDALDataset->srs = fileSRS;

    LOGA("open success: res -> %p, handle -> %p", pGDALDataset, hDataset);
    ERL_NIF_TERM res = enif_make_resource(env, pGDALDataset);
    enif_release_resource(pGDALDataset);
    return res;
}

static ERL_NIF_TERM info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    MyGDALDataset *pGDALDataset = NULL;
    if (!enif_get_resource(env, argv[0], gdalDatasetResType, (void**)&pGDALDataset)) {
        return enif_make_badarg(env);
    }
    GDALDatasetH hDataset = pGDALDataset->handle;
    LOGA("info success: res -> %p, handle -> %p", pGDALDataset, hDataset);
    GDALDriverH hDriver = GDALGetDatasetDriver(hDataset);
    ERL_NIF_TERM res = enif_make_new_map(env);
    enif_make_map_put(env, res, enif_make_atom(env, "driverShortName"),
            enif_make_string(env, GDALGetDriverShortName(hDriver), ERL_NIF_LATIN1),
            &res);
    enif_make_map_put(env, res, enif_make_atom(env, "driverLongName"),
            enif_make_string(env, GDALGetDriverLongName(hDriver), ERL_NIF_LATIN1),
            &res);
    enif_make_map_put(env, res, enif_make_atom(env, "raster_width"),
            enif_make_int(env, pGDALDataset->rasterWidth),
            &res);
    enif_make_map_put(env, res, enif_make_atom(env, "raster_height"),
            enif_make_int(env, pGDALDataset->rasterHeight),
            &res);
    enif_make_map_put(env, res, enif_make_atom(env, "bands"),
            enif_make_int(env, pGDALDataset->rasterCount),
            &res);
    const char* proj = GDALGetProjectionRef(hDataset);
    if (proj != NULL) {
        enif_make_map_put(env, res, enif_make_atom(env, "projection"),
                enif_make_string(env, proj, ERL_NIF_LATIN1),
                &res);
    }

    double adfGeoTransform[6];
    if (GDALGetGeoTransform(hDataset, adfGeoTransform) == CE_None) {
        enif_make_map_put(env, res, enif_make_atom(env, "origin"),
                enif_make_tuple2(env,
                    enif_make_double(env, pGDALDataset->originX),
                    enif_make_double(env, pGDALDataset->originY)),
                &res);
        enif_make_map_put(env, res, enif_make_atom(env, "pixel_size"),
                enif_make_tuple2(env,
                    enif_make_double(env, pGDALDataset->pixelWidth),
                    enif_make_double(env, pGDALDataset->pixelHeight)),
                &res);
    }
                    
    return res;
}

static ERL_NIF_TERM band_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    MyGDALDataset *pGDALDataset = NULL;
    if (!enif_get_resource(env, argv[0], gdalDatasetResType, (void**)&pGDALDataset)) {
        return enif_make_badarg(env);
    }
    int bandNo = 0;
    if (!enif_get_int(env, argv[1], &bandNo)) {
        return enif_make_badarg(env);
    }
    GDALDatasetH hDataset = pGDALDataset->handle;
    LOGA("band_info success: res -> %p, handle -> %p, for band# %d", pGDALDataset, hDataset, bandNo);
    GDALRasterBandH hBand = GDALGetRasterBand(hDataset, bandNo);

    ERL_NIF_TERM res = enif_make_new_map(env);
    int nBlockXSize, nBlockYSize;
    GDALGetBlockSize(hBand, &nBlockXSize, &nBlockYSize);
    enif_make_map_put(env, res, enif_make_atom(env, "block_size"),
            enif_make_tuple2(env,
                enif_make_int(env, nBlockXSize),
                enif_make_int(env, nBlockYSize)),
            &res);

    enif_make_map_put(env, res, enif_make_atom(env, "raster_width"),
                      enif_make_int(env, GDALGetRasterBandXSize(hBand)),
                      &res);
    enif_make_map_put(env, res, enif_make_atom(env, "raster_height"),
                      enif_make_int(env, GDALGetRasterBandYSize(hBand)),
                      &res);

    enif_make_map_put(env, res, enif_make_atom(env, "pixel_datatype"),
            enif_make_atom(env, rasterDataType(GDALGetRasterDataType(hBand))),
            &res);
    if (GDALGetRasterColorTable(hBand) != NULL) {
        WARNA("Please convert this file to RGB/RGBA by gdal_translate");
        return enif_raise_exception(env,
                enif_make_string(env, "cannot do color table", ERL_NIF_LATIN1));

    }
    int bGotMin, bGotMax;
    double adfMinMax[2] = {
        GDALGetRasterMinimum(hBand, &bGotMin),
        GDALGetRasterMaximum(hBand, &bGotMax),
    };
    if (!(bGotMin && bGotMax)) {
        WARNA("lengthy compute....");
        GDALComputeRasterMinMax(hBand, TRUE, adfMinMax);
    }
    enif_make_map_put(env, res,
            enif_make_atom(env, "min_max"),
            enif_make_list2(env,
                enif_make_double(env, adfMinMax[0]),
                enif_make_double(env, adfMinMax[1])),
            &res);
    
    return res;
}

static ERL_NIF_TERM get_pixel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    MyGDALDataset *pGDALDataset = NULL;
    if (!enif_get_resource(env, argv[0], gdalDatasetResType, (void**)&pGDALDataset)) {
        return enif_make_badarg(env);
    }
    int nXOff;
    if (!enif_get_int(env, argv[1], &nXOff)) {
        return enif_make_badarg(env);
    }
    int nYOff;
    if (!enif_get_int(env, argv[2], &nYOff)) {
        return enif_make_badarg(env);
    }
    GDALDatasetH hDataset = pGDALDataset->handle;
    int bandNo = 1;
    GDALRasterBandH hBand = GDALGetRasterBand(hDataset, bandNo);

    float pixelValue;
    CPLErr res = GDALRasterIO(hBand, GF_Read,
            nXOff, nYOff, 1, 1,
            &pixelValue, 1, 1,
            GDT_Float32,
            0,0);
    if (res != CE_None) {
        return enif_raise_exception(env,
                enif_make_string(env, "raster io error", ERL_NIF_LATIN1));
    }
    return enif_make_double(env, pixelValue);
}

static int nifload(ErlNifEnv* env, void **priv_data, ERL_NIF_TERM load_info) {
    GDALAllRegister();
    CPLSetErrorHandler(MyGDALErrorHandler);

    gdalDatasetResType = enif_open_resource_type(env, NULL, "gdalnif2tiles", dataset_dtor,
            ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER, NULL);
    ATOM_OK = enif_make_atom(env, "ok");
    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"open_file", 1, open_file, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"info", 1, info, 0},
    {"band_info", 2, band_info, 0},
    {"get_pixel", 3, get_pixel, 0}
};

ERL_NIF_INIT(gdalnif2tiles, nif_funcs, nifload, NULL,NULL,NULL)
