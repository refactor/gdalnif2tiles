#include "mylog.h"

#include <erl_nif.h>
#include <gdal.h>
#include <cpl_conv.h>
#include <cpl_vsi.h>

#include <ogr_srs_api.h>

#include "utils.h"

void MyGDALErrorHandler(CPLErr eErrClass, int errNo, const char *msg) {
    if (eErrClass <= CE_Warning) {
        WARN_LOG("GDAL.errno: %d, %s", errNo, msg);
    } else {
        ERR_LOG("GDAL.errno: %d, %s", errNo, msg);
    }
}

#define ENIF(name) static ERL_NIF_TERM name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])

static ERL_NIF_TERM ATOM_OK;

static ErlNifResourceType* profileResType;

void profile_dtor(ErlNifEnv* env, void* obj) {
    WorldProfile *profile = (WorldProfile*)obj;
    if (profile->outputSRS != NULL) {
        OSRDestroySpatialReference(profile->outputSRS);
        profile->outputSRS = NULL;
    }
}

static ErlNifResourceType* gdalDatasetResType;

void dataset_dtor(ErlNifEnv* env, void* obj) {
    MyGDALDataset *pGDALDataset = (MyGDALDataset*)obj;
    LOGA("pGDALDataset -> %p, handle -> %p, profile -> %p", pGDALDataset, pGDALDataset->handle, pGDALDataset->profile);
    if (pGDALDataset) {
        if (pGDALDataset->warped_input_dataset != NULL) {
            if (pGDALDataset->warped_input_dataset != pGDALDataset->handle) {
                LOGA("close warped_input_dataset: %p", pGDALDataset->warped_input_dataset);
                GDALClose(pGDALDataset->warped_input_dataset);
            }
            pGDALDataset->warped_input_dataset = NULL;
        }
        if (pGDALDataset->memFile != NULL) {
            LOGA("close memFile: %p", pGDALDataset->memFile);
            VSIFCloseL(pGDALDataset->memFile);
            pGDALDataset->memFile = NULL;
        }
        if (pGDALDataset->handle != NULL) {
            GDALClose(pGDALDataset->handle);
            pGDALDataset->handle = NULL;
        }
        if (pGDALDataset->inputSRS != NULL) {
            OSRDestroySpatialReference(pGDALDataset->inputSRS);
            pGDALDataset->inputSRS = NULL;
        }
        if (pGDALDataset->in_nodata != NULL) {
            enif_free(pGDALDataset->in_nodata);
            pGDALDataset->in_nodata = NULL;
        }
        if (pGDALDataset->profile != NULL) {
            enif_release_resource((void*)pGDALDataset->profile);
            pGDALDataset->profile = NULL;
        }
    }
}

ENIF(create_profile) {
    LOGA("argv: %T", argv[0]);
    char buf[32];
    if (!enif_get_atom(env, argv[0], buf, sizeof(buf), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    world_profile_type profileType = PROFILE_TYPE_COUNT;
    for (int i=0; i<PROFILE_TYPE_COUNT; ++i) {
        if (strncasecmp(WORLD_PROFILE_TYPES[i], buf, sizeof(buf)) == 0) {
            profileType = i;
            break;
        }
    }
    if (profileType == PROFILE_TYPE_COUNT) {
        return enif_make_badarg(env);
    }

    WorldProfile *profile = enif_alloc_resource(profileResType, sizeof(*profile));
    createProfile(profileType, profile);
    ERL_NIF_TERM res = enif_make_resource(env, profile);
    enif_release_resource(profile);
    return res;
}

ENIF(open_file) {
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
    *pGDALDataset = (MyGDALDataset){0};
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
    
    const char* proj = GDALGetProjectionRef(pGDALDataset->handle);
    OGRSpatialReferenceH fileSRS = OSRNewSpatialReference(NULL);
    if (proj == NULL || OGRERR_NONE != OSRSetFromUserInput(fileSRS, proj)) {
        OSRDestroySpatialReference(fileSRS);
        return enif_raise_exception(env,
                enif_make_string(env, "NO spatial reference found", ERL_NIF_LATIN1));
    }
    pGDALDataset->inputSRS = fileSRS;

    nodata_list *nodata = (nodata_list*) enif_alloc(
            sizeof(*nodata) + pGDALDataset->rasterCount*sizeof(double));
    nodata->len = pGDALDataset->rasterCount;
    for (int i = 1; i <= pGDALDataset->rasterCount; ++i) {
        int successFlag = 0;
        GDALRasterBandH hBand = GDALGetRasterBand(pGDALDataset->handle, i);
        nodata->nodata[i] = GDALGetRasterNoDataValue(hBand, &successFlag);
        LOGA("band.%d nodata: %f", i, nodata->nodata[i]);
        if (!successFlag) WARNA("band.%d: no-data found...", i);
    }
    pGDALDataset->in_nodata = nodata;

    LOGA("open success: res -> %p, handle -> %p", pGDALDataset, hDataset);
    ERL_NIF_TERM res = enif_make_resource(env, pGDALDataset);
    enif_release_resource(pGDALDataset);
    return res;
}

ENIF(has_nodata) {
    MyGDALDataset *pGDALDataset = NULL;
    if (!enif_get_resource(env, argv[0], gdalDatasetResType, (void**)&pGDALDataset)) {
        return enif_make_badarg(env);
    }
    if (pGDALDataset->in_nodata != NULL) {
        return enif_make_atom(env, "true");
    } 
    return enif_make_atom(env, "false");
}

ENIF(info) {
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

ENIF(band_info) {
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

ENIF(reproj2profile) {
    MyGDALDataset *pGDALDataset = NULL;
    if (!enif_get_resource(env, argv[0], gdalDatasetResType, (void**)&pGDALDataset)) {
        return enif_make_badarg(env);
    }
    WorldProfile *profile = NULL;
    if (!enif_get_resource(env, argv[1], profileResType, (void**)&profile)) {
        return enif_make_badarg(env);
    }
    reprojectWithProfile(pGDALDataset, profile);

    return argv[0];
}

ENIF(correct_dataset) {
    MyGDALDataset *pGDALDataset = NULL;
    if (!enif_get_resource(env, argv[0], gdalDatasetResType, (void**)&pGDALDataset)) {
        return enif_make_badarg(env);
    }
    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }
    char *filename = "/vsimem/tiles.mem";
    uint8_t *data = malloc(bin.size);
    memcpy(data, bin.data, bin.size);
    VSILFILE *memFile = VSIFileFromMemBuffer(filename, data, bin.size, TRUE);
    const char* nodatavalues = cat_novalues(pGDALDataset->in_nodata);
    LOGA("nodata: %s", nodatavalues);
    GDALDatasetH correctedDataset = GDALOpen(filename, GA_ReadOnly);
    LOGA("correctedDataset: %p", correctedDataset);
    if (CE_None != GDALSetMetadataItem(correctedDataset, "NODATA_VALUES", nodatavalues, NULL)) {
        free((void*)nodatavalues);
        return enif_raise_exception(env, enif_make_string(env, "fail to set metadata", ERL_NIF_LATIN1));
    }
    free((void*)nodatavalues);
    pGDALDataset->warped_input_dataset = correctedDataset;
    pGDALDataset->memFile = memFile;
    return argv[0];
}

ENIF(get_xmlvrt) {
    MyGDALDataset *pGDALDataset = NULL;
    if (!enif_get_resource(env, argv[0], gdalDatasetResType, (void**)&pGDALDataset)) {
        return enif_make_badarg(env);
    }
    if (pGDALDataset->warped_input_dataset == NULL) {
        return enif_raise_exception(env, enif_make_string(env, "no warped input dataset found", ERL_NIF_LATIN1));
    }
    char** md = GDALGetMetadata(pGDALDataset->warped_input_dataset, "xml:VRT");
    if (md == NULL) {
        return enif_raise_exception(env, enif_make_string(env, "no xml:VRT found", ERL_NIF_LATIN1));
    }
    return enif_make_string(env, md[0], ERL_NIF_LATIN1);
}

ENIF(get_pixel) {
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

    profileResType = enif_open_resource_type(env, NULL, "worldProfile", profile_dtor,
            ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER, NULL);
    gdalDatasetResType = enif_open_resource_type(env, NULL, "gdalDataset", dataset_dtor,
            ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER, NULL);
    ATOM_OK = enif_make_atom(env, "ok");
    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"create_profile", 1, create_profile, 0},
    {"open_file",      1, open_file, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"has_nodata",     1, has_nodata, 0},
    {"reproj2profile", 2, reproj2profile, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"correct_dataset",2, correct_dataset, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"get_xmlvrt",     1, get_xmlvrt, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"info",           1, info, 0},
    {"band_info",      2, band_info, 0},
    {"get_pixel",      3, get_pixel, 0}
};

ERL_NIF_INIT(gdalnif2tiles, nif_funcs, nifload, NULL,NULL,NULL)
