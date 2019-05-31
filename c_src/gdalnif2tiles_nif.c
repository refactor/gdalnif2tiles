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
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;

static ErlNifResourceType* profileResType;

void profile_dtor(ErlNifEnv* env, void* obj) {
    WorldProfile *profile = (WorldProfile*)obj;
    if (profile->output_srs != NULL) {
        OSRDestroySpatialReference(profile->output_srs);
        profile->output_srs = NULL;
    }
}

static ErlNifResourceType* warpedDatasetResType;

static void warped_dataset_dtor(ErlNifEnv *env, void* obj) {
    WarpedDataset *warpedDataset = (WarpedDataset*)obj;
    LOG("warpedDataset -> %p", warpedDataset);
    if (warpedDataset->warped) {
        LOG("close warped_input_dataset: %p", warpedDataset->warped_input_dataset);
        GDALClose(warpedDataset->warped_input_dataset);
        warpedDataset->warped_input_dataset = NULL;
    }
    if (strncmp(warpedDataset->vmfilename, "/vsimem/", 8) == 0) {
        LOG("delete memFile: %s", warpedDataset->vmfilename);
        VSIUnlink(warpedDataset->vmfilename);
    }
    if (warpedDataset->profile) {
        LOG("release warped profile: %p", warpedDataset->profile);
        enif_release_resource((void*)warpedDataset->profile);
        warpedDataset->profile = NULL;
    }
}

ENIF(create_profile) {
    LOG("argv: %T", argv[0]);
    char buf[32] = {0};
    if (!enif_get_atom(env, argv[0], buf, sizeof(buf), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    profile_type profileType = PROFILE_TYPE_COUNT;
    for (int i=0; i<PROFILE_TYPE_COUNT; ++i) {
        if (strncasecmp(WORLD_PROFILE_TYPES[i], buf, sizeof(buf)) == 0) {
            profileType = i;
            break;
        }
    }
    if (profileType == PROFILE_TYPE_COUNT) {
        WARN("unknow profile_type: %s", buf);
        return enif_make_badarg(env);
    }

    WorldProfile *profile = enif_alloc_resource(profileResType, sizeof(*profile));
    *profile = (WorldProfile) { 0 };
    if (argc == 2) {
        LOG("TMS compatible...");
        profile->tmscompatible = true;
    }

    initProfile(profileType, profile);
    ERL_NIF_TERM res = enif_make_resource(env, profile);
    enif_release_resource(profile);
    return res;
}

nodata_list* extract_nodatavalues(const GDALDatasetH hDataset) {
    int rasterCount = GDALGetRasterCount(hDataset);
    // setup_no_data_values FROM inputfile:
    nodata_list *nodata = (nodata_list*) enif_alloc(sizeof(*nodata) + rasterCount*sizeof(double));
    nodata->bandCount = rasterCount;
    for (int i = 1; i <= rasterCount; ++i) {
        int successFlag = 0;
        GDALRasterBandH hBand = GDALGetRasterBand(hDataset, i);
        nodata->nodata[i - 1] = GDALGetRasterNoDataValue(hBand, &successFlag);
        LOG("band.%d NODATA: %f, sucess: %d", i, nodata->nodata[i - 1], successFlag);
        if (!successFlag) {
            WARN("band.%d: NOT sucess get NODATA ... BUT get this: %f", i, nodata->nodata[i - 1]);
            enif_free(nodata);
            return NULL;
        }
    }
    return nodata;
}

ENIF(has_nodata) {
    const WarpedDataset *wGDALDataset = NULL;
    if (!enif_get_resource(env, argv[0], warpedDatasetResType, (void**)&wGDALDataset)) {
        return enif_make_badarg(env);
    }
    if (wGDALDataset->nodata == NULL)
        return ATOM_FALSE;
    return ATOM_TRUE;
}

ENIF(nb_data_bands) {
    const WarpedDataset *wGDALDataset = NULL;
    if (!enif_get_resource(env, argv[0], warpedDatasetResType, (void**)&wGDALDataset)) {
        return enif_make_badarg(env);
    }
    GDALDatasetH hDataset = NULL;
    if (wGDALDataset) hDataset = wGDALDataset->warped_input_dataset;

    const int bandNo = 1;
    GDALRasterBandH hBand = GDALGetRasterBand(hDataset, bandNo);
    GDALRasterBandH alphaband = GDALGetMaskBand(hBand);
    int rasterCount = GDALGetRasterCount(hDataset);
    if ((GDALGetMaskFlags(alphaband) & GMF_ALPHA) ||
        rasterCount == 4 || rasterCount == 2) {
        rasterCount -= 1;
    }
    return enif_make_int(env, rasterCount);
}

ENIF(info) {
    const WarpedDataset *wGDALDataset = NULL;
    if (!enif_get_resource(env, argv[0], warpedDatasetResType, (void**)&wGDALDataset)) {
        return enif_make_badarg(env);
    }
    GDALDatasetH hDataset = NULL;
    if (wGDALDataset) hDataset = wGDALDataset->warped_input_dataset;

    LOG("info success, hDataset -> %p", hDataset);
    GDALDriverH hDriver = GDALGetDatasetDriver(hDataset);
    ERL_NIF_TERM res = enif_make_new_map(env);
    enif_make_map_put(env, res, enif_make_atom(env, "driverShortName"),
            enif_make_string(env, GDALGetDriverShortName(hDriver), ERL_NIF_LATIN1),
            &res);
    enif_make_map_put(env, res, enif_make_atom(env, "driverLongName"),
            enif_make_string(env, GDALGetDriverLongName(hDriver), ERL_NIF_LATIN1),
            &res);
    enif_make_map_put(env, res, enif_make_atom(env, "rasterWidth"),
            enif_make_int(env, GDALGetRasterXSize(hDataset)),
            &res);
    enif_make_map_put(env, res, enif_make_atom(env, "rasterHeight"),
            enif_make_int(env, GDALGetRasterYSize(hDataset)),
            &res);
    enif_make_map_put(env, res, enif_make_atom(env, "bandCount"),
            enif_make_int(env, GDALGetRasterCount(hDataset)),
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
                    enif_make_double(env, adfGeoTransform[0]),
                    enif_make_double(env, adfGeoTransform[3])),
                &res);
        enif_make_map_put(env, res, enif_make_atom(env, "pixelSize"),
                enif_make_tuple2(env,
                    enif_make_double(env, adfGeoTransform[1]),
                    enif_make_double(env, adfGeoTransform[5])),
                &res);
    }
                    
    if (wGDALDataset->nodata) {
        char nodatavalues[128] = {0};
        cat_novalues(wGDALDataset->nodata, nodatavalues, sizeof(nodatavalues));
        enif_make_map_put(env, res, enif_make_atom(env, "nodataValues"),
                enif_make_string(env, nodatavalues, ERL_NIF_LATIN1),
                &res);
    }
    return res;
}

ENIF(band_info) {
    const WarpedDataset *wGDALDataset = NULL;
    if (!enif_get_resource(env, argv[0], warpedDatasetResType, (void**)&wGDALDataset)) {
        return enif_make_badarg(env);
    }
    GDALDatasetH hDataset = NULL;
    if (wGDALDataset) hDataset = wGDALDataset->warped_input_dataset;

    int bandNo = 0;
    if (!enif_get_int(env, argv[1], &bandNo)) {
        return enif_make_badarg(env);
    }
    LOG("band_info success: handle -> %p, for band# %d", hDataset, bandNo);
    GDALRasterBandH hBand = GDALGetRasterBand(hDataset, bandNo);

    ERL_NIF_TERM res = enif_make_new_map(env);
    int nBlockXSize, nBlockYSize;
    GDALGetBlockSize(hBand, &nBlockXSize, &nBlockYSize);
    enif_make_map_put(env, res, enif_make_atom(env, "blockSize"),
            enif_make_tuple2(env,
                enif_make_int(env, nBlockXSize),
                enif_make_int(env, nBlockYSize)),
            &res);

    enif_make_map_put(env, res, enif_make_atom(env, "rasterWidth"),
                      enif_make_int(env, GDALGetRasterBandXSize(hBand)),
                      &res);
    enif_make_map_put(env, res, enif_make_atom(env, "rasterHeight"),
                      enif_make_int(env, GDALGetRasterBandYSize(hBand)),
                      &res);

    enif_make_map_put(env, res, enif_make_atom(env, "pixelDatatype"),
            enif_make_atom(env, rasterDataType(GDALGetRasterDataType(hBand))),
            &res);
    int bGotMin, bGotMax;
    double adfMinMax[2] = {
        GDALGetRasterMinimum(hBand, &bGotMin),
        GDALGetRasterMaximum(hBand, &bGotMax),
    };
    if (!(bGotMin && bGotMax)) {
        WARN("lengthy compute....");
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

static inline WarpedDataset* reprojectTo(WarpedDataset *warpedDataset, const GDALDatasetH ds, const WorldProfile *destProfile) {
    warpedDataset->warped_input_dataset = reprojectDataset(ds, destProfile->output_srs);
    if (warpedDataset->warped_input_dataset == NULL) {
        return NULL;
    }

    if (warpedDataset->warped_input_dataset != ds) {
        warpedDataset->warped = true;
    }
    warpedDataset->nodata = extract_nodatavalues(ds);

    char** files = GDALGetFileList(ds);
    while (*files) LOG("GDALGetFileList: %s", *files++);

    warpedDataset->profile = destProfile;
    enif_keep_resource((void*)destProfile);
    return warpedDataset;
}

ENIF(reproj_with_profile) {
    char filename[128] = {0};
    if (!enif_get_string(env, argv[0], filename, sizeof(filename), ERL_NIF_LATIN1))
        return enif_raise_exception(env,
            enif_make_string(env, "No input file was specified", ERL_NIF_LATIN1));
    
    LOG("opening... CPLIsFilenameRelative: %d", CPLIsFilenameRelative(filename));
    LOG("CPLFormFilename:%s", CPLFormFilename(NULL, filename, NULL));
    GDALDatasetH hDataset = GDALOpen(filename, GA_ReadOnly);
    LOG("hDataset -> %p", hDataset);
    if (hDataset == NULL) {
        return enif_raise_exception(env,
            enif_make_string(env, "It is not possible to open the input file", ERL_NIF_LATIN1));
    }

    WorldProfile *profile = NULL;
    if (!enif_get_resource(env, argv[1], profileResType, (void**)&profile)) {
        return enif_make_badarg(env);
    }

    WarpedDataset *warpedDataset = enif_alloc_resource(warpedDatasetResType, sizeof(*warpedDataset));
    *warpedDataset = (WarpedDataset) { 0 };
    if (reprojectTo(warpedDataset, hDataset, profile) == NULL) 
        return enif_raise_exception(env,
            enif_make_string(env, "SpatialReference error", ERL_NIF_LATIN1));
    ERL_NIF_TERM res = enif_make_resource(env, warpedDataset);
    enif_release_resource(warpedDataset);
    return res;
}

ENIF(correct_dataset) {
    WarpedDataset *warpedDataset = NULL;
    if (!enif_get_resource(env, argv[0], warpedDatasetResType, (void**)&warpedDataset)) {
        WARN("fail to get warped dataset");
        return enif_make_badarg(env);
    }
    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin)) {
        WARN("xml:VRT: failed");
        return enif_make_badarg(env);
    }

    // mimic the python API: gdal.Open(vrt_string)
    const char *filename = "/vsimem/tmp/tiles.vrt";
    LOG("filename: %s", filename);
    uint8_t *vrt_string = malloc(bin.size);
    memcpy(vrt_string, bin.data, bin.size);
    //LOG("vrt_string: %s", vrt_string);
    VSIFCloseL( VSIFileFromMemBuffer(filename, vrt_string, bin.size, TRUE) );
    // no deed to free vrt_string, because the last TRUE means
    // the memory file system handler will take ownership of vrt_string, freeing it when the file is deleted.
    GDALDatasetH correctedDataset = GDALOpen(filename, GA_ReadOnly);

    if (warpedDataset->nodata != NULL) {
        char nodatavalues[128] = {0};
        cat_novalues(warpedDataset->nodata, nodatavalues, sizeof(nodatavalues));
        if (CE_None != GDALSetMetadataItem(correctedDataset, "NODATA_VALUES", (const char*)nodatavalues, NULL)) {
            VSIUnlink(filename);
            GDALClose(correctedDataset);
            return enif_raise_exception(env, enif_make_string(env, "fail to set metadata", ERL_NIF_LATIN1));
        }
    }

    GDALClose(warpedDataset->warped_input_dataset);  // ???
    warpedDataset->warped_input_dataset = correctedDataset;
    strncpy(warpedDataset->vmfilename, filename, sizeof(warpedDataset->vmfilename));
    return argv[0];
}

ENIF(get_xmlvrt) {
    WarpedDataset *warpedDataset = NULL;
    if (!enif_get_resource(env, argv[0], warpedDatasetResType, (void**)&warpedDataset)) {
        WARN("fail to get warped dataset");
        return enif_make_badarg(env);
    }
    char** md = GDALGetMetadata(warpedDataset->warped_input_dataset, "xml:VRT");
    if (md == NULL) {
        return enif_raise_exception(env, enif_make_string(env, "no xml:VRT found", ERL_NIF_LATIN1));
    }
    return enif_make_string(env, md[0], ERL_NIF_LATIN1);
}

ENIF(tile_bounds) {
    WorldProfile *profile = NULL;
    if (!enif_get_resource(env, argv[0], profileResType, (void**)&profile)) {
        return enif_make_badarg(env);
    }
    int tx;
    if (!enif_get_int(env, argv[1], &tx)) {
        return enif_make_badarg(env);
    }
    int ty;
    if (!enif_get_int(env, argv[2], &ty)) {
        return enif_make_badarg(env);
    }
    int tz;
    if (!enif_get_int(env, argv[3], &tz)) {
        return enif_make_badarg(env);
    }
    double bounds[4] = {0};
    tileBounds(profile, tx, ty, tz, bounds);
    ERL_NIF_TERM bs[4];
    for (int i=0; i<4; ++i) {
        bs[i] = enif_make_double(env, bounds[i]);
    }
    return enif_make_tuple_from_array(env, bs, 4);
}

ENIF(get_pixel) {
    WarpedDataset *warpedDataset = NULL;
    if (!enif_get_resource(env, argv[0], warpedDatasetResType, (void**)&warpedDataset)) {
        WARN("fail to get warped dataset");
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
    GDALDatasetH hDataset = warpedDataset->warped_input_dataset;
    int bandNo = 1;
    GDALRasterBandH hBand = GDALGetRasterBand(hDataset, bandNo);

    int8_t pixelValue;
    CPLErr res = GDALRasterIO(hBand, GF_Read,
            nXOff, nYOff, 1, 1,
            &pixelValue, 1, 1,
            GDT_Byte,
            0,0);
    if (res != CE_None) {
        return enif_raise_exception(env,
                enif_make_string(env, "raster io error", ERL_NIF_LATIN1));
    }
    return enif_make_int(env, pixelValue);
}

static int nifload(ErlNifEnv* env, void **priv_data, ERL_NIF_TERM load_info) {
    GDALAllRegister();
    CPLSetErrorHandler(MyGDALErrorHandler);
    const char* gdalRuntimeReleaseName = GDALVersionInfo("GDAL_RELEASE_NAME");
    const char* gdalRuntimeVersionNum  = GDALVersionInfo("VERSION_NUM");
    LOG("GDAL release name %s", gdalRuntimeReleaseName);
    LOG("GDAL version num: %s", gdalRuntimeVersionNum);
    int res = GDALCheckVersion(2, 4, "WHAT the hell..., egdal2tiles");
    LOG("check 2.4 version: %d", res);
    res = GDALCheckVersion(3, 0, "what the hell...");
    LOG("check 3.0 version: %d", res);

    profileResType = enif_open_resource_type(env, NULL, "worldProfile", profile_dtor,
            ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER, NULL);
    warpedDatasetResType = enif_open_resource_type(env, NULL, "warpedDataset", warped_dataset_dtor, ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER, NULL);

    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_TRUE = enif_make_atom(env, "true");
    ATOM_FALSE = enif_make_atom(env, "false");
    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"create_profile", 1, create_profile, 0},
    {"create_profile", 2, create_profile, 0},
    {"has_nodata",     1, has_nodata, 0},
    {"reproj_with_profile", 2, reproj_with_profile, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"correct_dataset",2, correct_dataset, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"get_xmlvrt",     1, get_xmlvrt, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"nb_data_bands",  1, nb_data_bands, 0},
    {"info",           1, info, 0},
    {"band_info",      2, band_info, 0},
    {"tile_bounds",    4, tile_bounds, 0},
    {"get_pixel",      3, get_pixel, 0}
};

ERL_NIF_INIT(gdalnif2tiles, nif_funcs, nifload, NULL,NULL,NULL)
