#include "mylog.h"
#include <erl_nif.h>

#include <gdal.h>
#include <cpl_conv.h>
#include <cpl_vsi.h>
#include <cpl_string.h>

#include <ogr_srs_api.h>

#include "utils.h"

void MyGDALErrorHandler(CPLErr eErrClass, int errNo, const char *msg) {
    if (eErrClass <= CE_Warning) {
        WARN_LOG("GDAL.errno: %d, %s", errNo, msg);
    } else {
        ERR_LOG("GDAL.errno: %d, %s", errNo, msg);
    }
}

#define ENIF(name) static ERL_NIF_TERM name(ErlNifEnv* env, int __attribute__((unused)) argc, const ERL_NIF_TERM argv[])

static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;

static ERL_NIF_TERM ATOM_PROFILE_MERCATOR;
static ERL_NIF_TERM ATOM_PROFILE_GEODETIC;

static ErlNifResourceType* warpedDatasetResType;
static ErlNifResourceType* tiledDatasetResType;
static ErlNifResourceType* assemblypartResType;

static inline bool is_warped_dataset(const WarpedDataset *warpedDataset) {
    return warpedDataset && warpedDataset->warped_input_dataset &&
        (warpedDataset->raw_input_dataset != warpedDataset->warped_input_dataset);
}

static void warped_dataset_dtor(ErlNifEnv * __attribute__((unused)) env, void* obj) {
    WarpedDataset *warpedDataset = (WarpedDataset*)obj;
    INFO_LOG("destroy warpedDataset -> %p", warpedDataset);
    if (is_warped_dataset(warpedDataset) && warpedDataset->warped_input_dataset) {
        DBG("close warped_input_dataset: %p", warpedDataset->warped_input_dataset);
        GDALClose(warpedDataset->warped_input_dataset);
    }
    warpedDataset->warped_input_dataset = NULL;

    if (strncmp(warpedDataset->vmfilename, "/vsimem/", 8) == 0) {
        DBG("delete memFile: %s", warpedDataset->vmfilename);
        VSIUnlink(warpedDataset->vmfilename);
    }
    if (warpedDataset->output_srs) {
        OSRDestroySpatialReference(warpedDataset->output_srs);
        warpedDataset->output_srs = NULL;
    }
    if (warpedDataset->raw_input_dataset) {
        DBG("close raw_input_dataset: %p", warpedDataset->raw_input_dataset);
        GDALClose(warpedDataset->raw_input_dataset);
        warpedDataset->raw_input_dataset = NULL;
    }
    if (warpedDataset->nodata) {
        DBG("free nodata...%p", warpedDataset->nodata);
        enif_free(warpedDataset->nodata);
        warpedDataset->nodata = NULL;
    }
}

typedef struct tiled_dataset {
    GDALDatasetH dstile; // MEM tile
    uint32_t tx;
    uint32_t ty;
    uint32_t tz;
    bool transparent;
} tiled_dataset;

static void tiled_dataset_dtor(ErlNifEnv * __attribute__((unused)) env, void* obj) {
    tiled_dataset *tds = (tiled_dataset*)obj;
    INFO_LOG("destroy tiled_dataset -> %p", obj);
    if (tds->dstile) {
        DBG("close tiled dataset -> ", tds->dstile);
        GDALClose(tds->dstile);
        tds->dstile = NULL;
    }
}

typedef struct tiled_dataset_assemblypart {
    uint32_t tx;
    uint32_t ty;
    uint32_t tz;
    bool transparent;
    uint32_t wx;
    uint32_t wy;
    uint32_t wxsize;
    uint32_t wysize;
    uint32_t tile_size;
    uint32_t querysize;
    uint32_t dataBandsCount;
    uint32_t tilebands;
    GDALDataType datatype;
    GDALDataType alphatype;
    uint8_t* data;
    uint8_t* alpha;
} tiled_dataset_assemblypart;

static void tiled_dataset_parts_dtor(ErlNifEnv * __attribute__((unused)) env, void* obj) {
    tiled_dataset_assemblypart *part = (tiled_dataset_assemblypart*)obj;
    INFO_LOG("destroy tile_parts -> %p", obj);
    if (part->data) {
        DBG("free memory -> ", part->data);
        VSIFree(part->data);
        part->data = NULL;
    }
    if (part->alpha) {
    //    CPLFree(part->alpha);
        part->alpha = NULL;
    }
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
        DBG("band.%d NODATA: %f, sucess: %d", i, nodata->nodata[i - 1], successFlag);
        if (!successFlag) {
            WARN("band.%d: NOT sucess get NODATA ... BUT get this: %f", i, nodata->nodata[i - 1]);
            enif_free(nodata);
            return NULL;
        }
    }
    return nodata;
}

ENIF(is_warped) {
    const WarpedDataset *warpedDataset = NULL;
    if (!enif_get_resource(env, argv[0], warpedDatasetResType, (void**)&warpedDataset)) {
        return enif_make_badarg(env);
    }
    if (is_warped_dataset(warpedDataset))
        return ATOM_TRUE;
    return ATOM_FALSE;
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

static inline int count_data_bands(GDALDatasetH hDataset) {
    const int bandNo = 1;
    GDALRasterBandH hBand = GDALGetRasterBand(hDataset, bandNo);
    GDALRasterBandH alphaband = GDALGetMaskBand(hBand);
    int rasterCount = GDALGetRasterCount(hDataset);
    if ((GDALGetMaskFlags(alphaband) & GMF_ALPHA) ||
        rasterCount == 4 || rasterCount == 2) {
        rasterCount -= 1;
    }
    return rasterCount;
}

ENIF(nb_data_bands) {
    const WarpedDataset *wGDALDataset = NULL;
    if (!enif_get_resource(env, argv[0], warpedDatasetResType, (void**)&wGDALDataset)) {
        return enif_make_badarg(env);
    }
    GDALDatasetH hDataset = NULL;
    if (wGDALDataset) hDataset = wGDALDataset->warped_input_dataset;

    int rasterCount = count_data_bands(hDataset);
    return enif_make_int(env, rasterCount);
}

ENIF(write_png) {
    const tiled_dataset *tiledDataset = NULL;
    if (!enif_get_resource(env, argv[0], tiledDatasetResType, (void**)&tiledDataset)) {
        WARN("unknown info(%T)", argv[0]);
        return enif_make_badarg(env);
    }

    char output_dir[128];
    if (!enif_get_string(env, argv[1], output_dir, sizeof(output_dir), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }
    
    char filename[256];
    snprintf(filename, sizeof(filename), "%s/%d-%d-%d.png", output_dir, tiledDataset->tz, tiledDataset->tx, tiledDataset->ty);
    //snprintf(filename, sizeof(filename), "%d.png", tiledDataset->ty);
    DBG("filename: %s", filename);
    GDALDriverH pngDriver = GDALGetDriverByName("PNG");
    GDALDatasetH hDstDS = GDALCreateCopy(pngDriver, filename, tiledDataset->dstile, FALSE, NULL, NULL, NULL);
    if (hDstDS != NULL) GDALClose(hDstDS);

    return ATOM_OK;
}

ENIF(dataset_info) {
    const WarpedDataset *wGDALDataset = NULL;
    const tiled_dataset *tiledDataset = NULL;
    if (!enif_get_resource(env, argv[0], warpedDatasetResType, (void**)&wGDALDataset) &&
        !enif_get_resource(env, argv[0], tiledDatasetResType, (void**)&tiledDataset)) {
        WARN("unknown dataset_info(%T)", argv[0]);
        return enif_make_badarg(env);
    }
    
    GDALDatasetH hDataset = NULL;
    if (wGDALDataset) {
        if (is_warped_dataset(wGDALDataset))
            hDataset = wGDALDataset->warped_input_dataset;
        else
            hDataset = wGDALDataset->raw_input_dataset;
    }
    if (tiledDataset) hDataset = tiledDataset->dstile;

    if (hDataset == NULL)
        return enif_raise_exception(env, enif_make_string(env, "NULL warped_input_dataset", ERL_NIF_LATIN1));

    DBG("dataset_info success, dataset -> %p", hDataset);
    GDALDriverH hDriver = GDALGetDatasetDriver(hDataset);
    ERL_NIF_TERM res = enif_make_new_map(env);
    enif_make_map_put(env, res, enif_make_atom(env, "driverShortName"),
            enif_make_string(env, GDALGetDriverShortName(hDriver), ERL_NIF_LATIN1),
            &res);
    enif_make_map_put(env, res, enif_make_atom(env, "driverLongName"),
            enif_make_string(env, GDALGetDriverLongName(hDriver), ERL_NIF_LATIN1),
            &res);

    enif_make_map_put(env, res, enif_make_atom(env, "rasterSize"),
            enif_make_tuple2(env, 
                enif_make_int(env, GDALGetRasterXSize(hDataset)),
                enif_make_int(env, GDALGetRasterYSize(hDataset))),
            &res);

    enif_make_map_put(env, res, enif_make_atom(env, "bandCount"),
            enif_make_int(env, GDALGetRasterCount(hDataset)),
            &res);
    enif_make_map_put(env, res, enif_make_atom(env, "dataBandsCount"),
            enif_make_int(env, count_data_bands(hDataset)),
            &res);

    const char* proj = GDALGetProjectionRef(hDataset);
    if (proj != NULL && CPLStrnlen(proj, 16) > 0) {
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
                    
    if (wGDALDataset && wGDALDataset->nodata) {
        char nodatavalues[128] = {0};
        cat_novalues(wGDALDataset->nodata, nodatavalues, sizeof(nodatavalues));
        enif_make_map_put(env, res, enif_make_atom(env, "nodataValues"),
                enif_make_string(env, nodatavalues, ERL_NIF_LATIN1),
                &res);
    }

    VSIStatBufL statBuf;
    DBG("GetFileList....");
    CSLConstList files = GDALGetFileList(hDataset);
    int filecount = CSLCount(files);
    ERL_NIF_TERM filenames[filecount];
    for (int i = 0; i < filecount; ++i) {
        const char* fn = CSLGetField(files, i);
        if (CE_None == VSIStatExL(fn, &statBuf, 0)) {
            filenames[i] = enif_make_tuple2(env, 
                    enif_make_string(env, fn, ERL_NIF_LATIN1),
                    enif_make_uint(env, statBuf.st_size));
        }
    }
    CSLDestroy(files);
    enif_make_map_put(env, res, enif_make_atom(env, "basefiles"),
            enif_make_list_from_array(env, filenames, filecount),
            &res);

    enif_make_map_put(env, res, enif_make_atom(env, "warped_input_dataset"),
            argv[0],
            &res);

    return res;
}

ENIF(band_info) {
    const WarpedDataset *wGDALDataset = NULL;
    const tiled_dataset *tiledDataset = NULL;
    if (!enif_get_resource(env, argv[0], warpedDatasetResType, (void**)&wGDALDataset) &&
        !enif_get_resource(env, argv[0], tiledDatasetResType, (void**)&tiledDataset)) {
        WARN("unknown dataset_info(%T)", argv[0]);
        return enif_make_badarg(env);
    }
    
    GDALDatasetH hDataset = NULL;
    if (wGDALDataset) hDataset = wGDALDataset->warped_input_dataset;
    if (tiledDataset) hDataset = tiledDataset->dstile;

    int bandNo = 0;
    if (!enif_get_int(env, argv[1], &bandNo)) {
        return enif_make_badarg(env);
    }
    DBG("success: warped_input_dataset -> %p, for band# %d", hDataset, bandNo);
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
            enif_make_atom(env, GDALGetDataTypeName(GDALGetRasterDataType(hBand))),
            &res);
    int bGotMin, bGotMax;
    double adfMinMax[2] = {
        GDALGetRasterMinimum(hBand, &bGotMin),
        GDALGetRasterMaximum(hBand, &bGotMax),
    };
    if (!(bGotMin && bGotMax)) {
        WARN("do lengthy compute.... [%f, %f]", adfMinMax[0], adfMinMax[1]);
        GDALComputeRasterMinMax(hBand, FALSE, adfMinMax);
    }
    enif_make_map_put(env, res,
            enif_make_atom(env, "min_max"),
            enif_make_list2(env,
                enif_make_double(env, adfMinMax[0]),
                enif_make_double(env, adfMinMax[1])),
            &res);
    
    return res;
}

static inline WarpedDataset* reprojectTo(WarpedDataset *warpedDataset, const GDALDatasetH hSrsDS, const ERL_NIF_TERM profile) {
    OGRSpatialReferenceH output_srs = OSRNewSpatialReference(NULL);
    if (enif_compare(ATOM_PROFILE_MERCATOR, profile) == 0) {
        OSRImportFromEPSG(output_srs, 3857);
    }
    else if (enif_compare(ATOM_PROFILE_GEODETIC, profile) == 0) {
        OSRImportFromEPSG(output_srs, 4326);
    }
    else {
        WARN("unknow profile: %T", profile);
        OSRDestroySpatialReference(output_srs);
        return NULL;
    }

    warpedDataset->warped_input_dataset = reprojectDataset(hSrsDS, output_srs);
    if (warpedDataset->warped_input_dataset == NULL) {
        OSRDestroySpatialReference(output_srs);
        return NULL;
    }

    warpedDataset->raw_input_dataset = hSrsDS;
    DBG("warped: %d", is_warped_dataset(warpedDataset));
    warpedDataset->nodata = extract_nodatavalues(hSrsDS);

    warpedDataset->output_srs = output_srs;
    return warpedDataset;
}

ENIF(open_with_profile) {
    char filename[128] = {0};
    if (!enif_get_string(env, argv[0], filename, sizeof(filename), ERL_NIF_LATIN1))
        return enif_raise_exception(env,
            enif_make_string(env, "No input file was specified", ERL_NIF_LATIN1));
    
    DBG("opening... CPLIsFilenameRelative: %d", CPLIsFilenameRelative(filename));
    DBG("CPLFormFilename:%s", CPLFormFilename(NULL, filename, NULL));
    GDALDatasetH hDataset = GDALOpen(filename, GA_ReadOnly);
    DBG("raw hDataset -> %p", hDataset);
    if (hDataset == NULL) {
        return enif_raise_exception(env,
            enif_make_string(env, "It is not possible to open the input file", ERL_NIF_LATIN1));
    }

    ERL_NIF_TERM profile = argv[1];

    // it's better to auto-GC WarpedDataset resource
    WarpedDataset *warpedDataset = (WarpedDataset*)enif_alloc_resource(warpedDatasetResType, sizeof(*warpedDataset));
    *warpedDataset = (WarpedDataset) { 0 };
    ERL_NIF_TERM res = enif_make_resource(env, warpedDataset);
    enif_release_resource(warpedDataset);
    if (reprojectTo(warpedDataset, hDataset, profile) == NULL) 
        return enif_raise_exception(env,
            enif_make_string(env, "SpatialReference error", ERL_NIF_LATIN1));

    ERL_NIF_TERM uiterm = enif_make_unique_integer(env, ERL_NIF_UNIQUE_POSITIVE);
    int ui = 0;
    enif_get_int(env, uiterm, &ui);
    if (snprintf(warpedDataset->vmfilename, sizeof(warpedDataset->vmfilename), "/vsimem/tmp/%s-%d.vrt", CPLGetBasename(filename), ui) < 0) {
        return enif_raise_exception(env, enif_make_string(env, "filename too long", ERL_NIF_LATIN1));
    }
    DBG("open with vmfilename: %s", warpedDataset->vmfilename);

    return res;
}

ENIF(create_vrt_copy) {
    WarpedDataset *wGDALDataset = NULL;
    if (!enif_get_resource(env, argv[0], warpedDatasetResType, (void**)&wGDALDataset)) {
        return enif_make_badarg(env);
    }
    DBG("create-copy an VRT dataset on memfile: %s, is input dataset warped: %d", wGDALDataset->vmfilename, is_warped_dataset(wGDALDataset));
    if (strncmp(wGDALDataset->vmfilename, "/vsimem/", 8) == 0) {
        VSIStatBufL statBuf;
        if (CE_None == VSIStatExL(wGDALDataset->vmfilename, &statBuf, 0) && statBuf.st_size > 0) {
            WARN("exists file: %s, skip! ", wGDALDataset->vmfilename);
            return argv[0];
        }
        DBG("memfile: sz=%d, mode=%d", statBuf.st_size, statBuf.st_mode);
        GDALDriverH vrtDriver =  GDALGetDriverByName("VRT");
        GDALDatasetH hDstDS = GDALCreateCopy(vrtDriver,wGDALDataset->vmfilename, wGDALDataset->warped_input_dataset, FALSE,
                NULL, NULL, NULL);
        if (hDstDS == NULL) {
            return enif_raise_exception(env, enif_make_string(env, "fail to create vrt copy", ERL_NIF_LATIN1));
        }

        // reopen the VRT dataset, otherwise bad thing will happen, such for GDALGetFileList(...)
        GDALClose(hDstDS);
        hDstDS = GDALOpen(wGDALDataset->vmfilename, GA_ReadOnly);

        if (CE_None == VSIStatExL(wGDALDataset->vmfilename, &statBuf, 0)) {
            DBG("memfile: sz=%d, mode=%d", statBuf.st_size, statBuf.st_mode);
        }

        if (is_warped_dataset(wGDALDataset)) {
            GDALClose(wGDALDataset->warped_input_dataset);
        }
        wGDALDataset->warped_input_dataset = hDstDS;
    }
    return argv[0];
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
    const char *memfilename = warpedDataset->vmfilename;
    DBG("memfilename: %s", memfilename);
    uint8_t *vrt_string = malloc(bin.size);
    memcpy(vrt_string, bin.data, bin.size);
    //DBG("vrt_string: %s", vrt_string);
    VSIFCloseL( VSIFileFromMemBuffer(memfilename, vrt_string, bin.size, TRUE) );
    // no deed to free vrt_string, because the last TRUE means
    // the memory file system handler will take ownership of vrt_string, freeing it when the file is deleted.
    GDALDatasetH correctedDataset = GDALOpen(memfilename, GA_ReadOnly);

    if (warpedDataset->nodata != NULL) {
        char nodatavalues[128] = {0};
        cat_novalues(warpedDataset->nodata, nodatavalues, sizeof(nodatavalues));
        if (CE_None != GDALSetMetadataItem(correctedDataset, "NODATA_VALUES", (const char*)nodatavalues, NULL)) {
            VSIUnlink(memfilename);
            GDALClose(correctedDataset);
            return enif_raise_exception(env, enif_make_string(env, "fail to set metadata", ERL_NIF_LATIN1));
        }
    }

    DBG("close old warped_input_dataset(%p), use corrected one(%p)", warpedDataset->warped_input_dataset, correctedDataset);
    GDALClose(warpedDataset->warped_input_dataset); 
    warpedDataset->warped_input_dataset = correctedDataset;
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

static inline uint32_t get_mapvalue(ErlNifEnv *env, ERL_NIF_TERM map, const char* keyname) {
    ERL_NIF_TERM value;
    uint32_t v = UINT32_MAX;
    if (!enif_get_map_value(env, map, enif_make_atom(env, keyname), &value) ||
            !enif_get_uint(env, value, &v)) {
        WARN("value: %T", value);
        return UINT32_MAX;
    }
    return v;
}
static inline WarpedDataset* get_wdataset_res(ErlNifEnv *env, ERL_NIF_TERM map) {
    ERL_NIF_TERM res;
    if (!enif_get_map_value(env, map, enif_make_atom(env, "warped_input_dataset"), &res))
        return NULL;
    WarpedDataset *warpedDataset = NULL;
    if (!enif_get_resource(env, res, warpedDatasetResType, (void**)&warpedDataset)) {
        WARN("fail to get warped dataset");
        return NULL;
    }
    return warpedDataset;
}
    
ENIF(advise_read) {
    const uint32_t dataBandsCount = get_mapvalue(env, argv[0], "dataBandsCount");
    DBG("dataBandsCount: %u", dataBandsCount);
    WarpedDataset *warpedDataset = get_wdataset_res(env, argv[0]);
    GDALDatasetH ds = warpedDataset->warped_input_dataset;
    DBG("src GDALDataset: %p, DataType", ds);
    //#{tx => Tx, ty => Ty, tz => TZ, rx => RX, ry => RY, rxsize => RXSize, rysize => RYSize,
    //  wx => WX, wy => WY, wxsize => WXSize, wysize => WYSize,
    //  querysize => maps:get(querysize, RasterProfile, undefined)}.
    const uint32_t rx        = get_mapvalue(env, argv[1], "rx");
    const uint32_t ry        = get_mapvalue(env, argv[1], "ry");
    const uint32_t rxsize    = get_mapvalue(env, argv[1], "rxsize");
    const uint32_t rysize    = get_mapvalue(env, argv[1], "rysize");
    const uint32_t wx        = get_mapvalue(env, argv[1], "wx");
    const uint32_t wy        = get_mapvalue(env, argv[1], "wy");
    const uint32_t wxsize    = get_mapvalue(env, argv[1], "wxsize");
    const uint32_t wysize    = get_mapvalue(env, argv[1], "wysize");
    DBG("rx: %u, ry: %u, rxsize: %u, rysize: %u", rx, ry, rxsize, rysize);
    DBG("wx: %u, wy: %u, wxsize: %u, wysize: %u", wx, wy, wxsize, wysize);

    int panBandMap[dataBandsCount];
    for (uint32_t i = 0; i < dataBandsCount; ++i) panBandMap[i] = i + 1;
    if (CE_None != GDALDatasetAdviseRead(ds, 
                        rx, ry, rxsize, rysize,
                        wxsize, wysize,
                        GDT_Byte,
                        dataBandsCount, panBandMap,
                        NULL)) {
        WARN("Fail to advise read");
        return ATOM_FALSE;
    }

    return ATOM_OK;
}

ENIF(extract_base_tile) {
    const uint32_t tile_size = get_mapvalue(env, argv[0], "tileSize");
    const uint32_t querysize = get_mapvalue(env, argv[0], "querysize");
    if (querysize == UINT32_MAX)
        return enif_raise_exception(env, enif_make_string(env, "no querysize", ERL_NIF_LATIN1));

    const uint32_t dataBandsCount = get_mapvalue(env, argv[0], "dataBandsCount");
    DBG("tile_size: %u, querysize: %u, dataBandsCount: %u", tile_size, querysize, dataBandsCount);
    WarpedDataset *warpedDataset = get_wdataset_res(env, argv[0]);
    GDALDatasetH ds = warpedDataset->warped_input_dataset;
    DBG("src GDALDataset: %p, DataType", ds);
    //#{tx => Tx, ty => Ty, tz => TZ, rx => RX, ry => RY, rxsize => RXSize, rysize => RYSize,
    //  wx => WX, wy => WY, wxsize => WXSize, wysize => WYSize,
    //  querysize => maps:get(querysize, RasterProfile, undefined)}.
    const uint32_t tx        = get_mapvalue(env, argv[1], "tx");
    const uint32_t ty        = get_mapvalue(env, argv[1], "ty");
    const uint32_t tz        = get_mapvalue(env, argv[1], "tz");
    const uint32_t rx        = get_mapvalue(env, argv[1], "rx");
    const uint32_t ry        = get_mapvalue(env, argv[1], "ry");
    const uint32_t rxsize    = get_mapvalue(env, argv[1], "rxsize");
    const uint32_t rysize    = get_mapvalue(env, argv[1], "rysize");
    const uint32_t wx        = get_mapvalue(env, argv[1], "wx");
    const uint32_t wy        = get_mapvalue(env, argv[1], "wy");
    const uint32_t wxsize    = get_mapvalue(env, argv[1], "wxsize");
    const uint32_t wysize    = get_mapvalue(env, argv[1], "wysize");
    DBG("tx: %u, ty: %u, tz: %u", tx, ty, tz);
    DBG("rx: %u, ry: %u, rxsize: %u, rysize: %u", rx, ry, rxsize, rysize);
    DBG("wx: %u, wy: %u, wxsize: %u, wysize: %u", wx, wy, wxsize, wysize);

    const int tilebands = dataBandsCount + 1;

    // make dstile GC
    tiled_dataset_assemblypart *tda = enif_alloc_resource(assemblypartResType, sizeof(*tda));
    *tda = (tiled_dataset_assemblypart) {
        .tx = tx,
        .ty = ty,
        .tz = tz,
        .wx = wx,
        .wy = wy,
        .wxsize = wxsize,
        .wysize = wysize,
        .tile_size = tile_size,
        .querysize = querysize,
        .dataBandsCount = dataBandsCount,
        .tilebands = tilebands,
        .transparent = true,
        .datatype = GDT_Byte,
        .alphatype = GDT_Byte
    };
    ERL_NIF_TERM ret = enif_make_resource(env, tda);
    enif_release_resource(tda);

    if (rxsize == 0 || rysize == 0 || wxsize == 0 || wysize == 0) {
        WARN("empty tile(tx=%u,ty=%u,tz=%u), just skip it", tx, ty, tz);
        return ret;
    }

    GDALRasterBandH hBand = GDALGetRasterBand(ds, 1);
    GDALRasterBandH alphaband = GDALGetMaskBand(hBand);
    tda->datatype = GDALGetRasterDataType(hBand);
    tda->alphatype = GDALGetRasterDataType(alphaband);
    DBG("band1type=%s, alphatype=%s", GDALGetDataTypeName(tda->datatype), GDALGetDataTypeName(tda->alphatype));

    tda->data = (uint8_t*) CPLCalloc( wxsize * wysize * dataBandsCount * GDALGetDataTypeSizeBytes(tda->datatype) +
                wxsize * wysize * GDALGetDataTypeSizeBytes(tda->alphatype), 1);

    tda->alpha = (uint8_t*) ((uintptr_t)tda->data + wxsize * wysize * dataBandsCount * GDALGetDataTypeSizeBytes(tda->datatype));
    CPLErr res = GDALRasterIO(alphaband, GF_Read,
            rx, ry, rxsize, rysize,
            tda->alpha, wxsize, wysize,
            tda->alphatype,
            0,0);
    if (res != CE_None) {
        VSIFree(tda->data); tda->data = NULL;
        return enif_raise_exception(env,
                enif_make_tuple3(env,
                    enif_make_atom(env, "read_alphaband_error"),
                    enif_make_tuple2(env, enif_make_uint(env, tx), enif_make_uint(env, ty)),
                    enif_make_string(env, CPLGetLastErrorMsg(), ERL_NIF_LATIN1)));
    }
    uint32_t sum = 0;
    for (uint32_t i = 0; i < wxsize * wysize; ++i) sum += tda->alpha[i]; // TODO: GDT_Type must be consided
    if (sum == 0) {
        WARN("Detect totally transparent tile(tx=%u,ty=%u,tz=%u) and SHOULD skip its creation",tx,ty,tz);
        VSIFree(tda->data); tda->data = NULL; tda->alpha = NULL;
        return ret;
    }

//    tda->data = (uint8_t*) CPLCalloc(wxsize * wysize * dataBandsCount, GDALGetDataTypeSizeBytes(tda->datatype));
    int panBandMap[dataBandsCount];
    for (uint32_t i = 0; i < dataBandsCount; ++i) panBandMap[i] = i + 1;
    res = GDALDatasetRasterIO(ds, GF_Read, 
            rx, ry, rxsize, rysize,
            tda->data, wxsize, wysize,
            tda->datatype,
            dataBandsCount, panBandMap,
            0, 0, 0);
 //   DBG("data: %s", CPLBinaryToHex(wxsize * wysize, data));
    if (res == CE_Failure) {
        WARN("fail to read dataset raster. errno=%d", CPLGetLastErrorNo());
        VSIFree(tda->data); tda->data = NULL; tda->alpha = NULL;
        return enif_raise_exception(env,
                enif_make_tuple3(env,
                    enif_make_atom(env, "dataset_read_error"),
                    enif_make_tuple2(env, enif_make_uint(env, tx), enif_make_uint(env, ty)),
                    enif_make_string(env, CPLGetLastErrorMsg(), ERL_NIF_LATIN1)));
    }

    // if data:
    // TODO: 1. different GDT_Type; 2. try async read
    tda->transparent = false;
    return ret;
}

ENIF(build_tile) {
    const tiled_dataset_assemblypart *tda = NULL;
    if (!enif_get_resource(env, argv[0], assemblypartResType, (void**)&tda)) {
        WARN("fail to get assemblypart");
        return enif_make_badarg(env);
    }
    GDALDriverH memDriver = GDALGetDriverByName( "MEM" );
    GDALDatasetH dstile = GDALCreate(memDriver, "",
            tda->tile_size, tda->tile_size, tda->tilebands, tda->datatype, NULL);

    tiled_dataset *tds = enif_alloc_resource(tiledDatasetResType, sizeof(*tds));
    tds->dstile = dstile;
    tds->tx = tda->tx;
    tds->ty = tda->ty;
    tds->tz = tda->tz;
    ERL_NIF_TERM ret = enif_make_resource(env, tds);
    enif_release_resource(tds);

    if (tda->transparent) {
        WARN("empty tile(tx=%u,ty=%u,tz=%u), just skip it", tda->tx, tda->ty, tda->tz);
        return ret;
    }

    int panBandMap[tda->dataBandsCount];
    for (uint32_t i = 0; i < tda->dataBandsCount; ++i) panBandMap[i] = i + 1;
    CPLErr res;
    if (tda->tile_size == tda->querysize) {
        res = GDALDatasetRasterIO(dstile, GF_Write,
                tda->wx, tda->wy, tda->wxsize, tda->wysize,
                tda->data, tda->wxsize, tda->wysize,
                tda->datatype,
                tda->dataBandsCount, panBandMap,
                0, 0, 0);
        if (res == CE_Failure) {
            ERL_NIF_TERM reason = enif_make_string(env, CPLGetLastErrorMsg(), ERL_NIF_LATIN1);
            WARN("fail to write data to dstile: errno=%T", reason);
            return enif_raise_exception(env,
                    enif_make_tuple3(env,
                        enif_make_atom(env, "dstile_write_error"),
                        enif_make_tuple2(env, enif_make_uint(env, tda->tx), enif_make_uint(env, tda->ty)),
                        reason));
        }
        res = GDALDatasetRasterIO(dstile, GF_Write,
                tda->wx, tda->wy, tda->wxsize, tda->wysize,
                tda->alpha, tda->wxsize, tda->wysize,
                tda->alphatype,
                1, (int[]){ tda->tilebands },
                0, 0, 0);
        if (res == CE_Failure) {
            ERL_NIF_TERM reason = enif_make_string(env, CPLGetLastErrorMsg(), ERL_NIF_LATIN1);
            WARN("fail to write alpha to dstile. error=%T", reason);
            return enif_raise_exception(env,
                    enif_make_tuple3(env,
                        enif_make_atom(env, "dstile_alpha_write_error"),
                        enif_make_tuple2(env, enif_make_uint(env, tda->tx), enif_make_uint(env, tda->ty)),
                        reason));
        }
    }
    else {
        GDALDatasetH dsquery = GDALCreate(memDriver, "", tda->querysize, tda->querysize, tda->tilebands, tda->datatype, NULL);
        res = GDALDatasetRasterIO(dsquery, GF_Write,
                tda->wx, tda->wy, tda->wxsize, tda->wysize,
                tda->data, tda->wxsize, tda->wysize,
                tda->datatype,
                tda->dataBandsCount, panBandMap,
                0, 0, 0);
        if (res == CE_Failure) {
            ERL_NIF_TERM reason = enif_make_string(env, CPLGetLastErrorMsg(), ERL_NIF_LATIN1);
            GDALClose(dsquery);
            return enif_raise_exception(env,
                    enif_make_tuple3(env,
                        enif_make_atom(env, "dsquery_write_error"),
                        enif_make_tuple2(env, enif_make_uint(env, tda->tx), enif_make_uint(env, tda->ty)),
                        reason));
        }
        res = GDALDatasetRasterIO(dsquery, GF_Write,
                tda->wx, tda->wy, tda->wxsize, tda->wysize,
                tda->alpha, tda->wxsize, tda->wysize,
                tda->alphatype,
                1, (int[]){ tda->tilebands },
                0, 0, 0);
        if (res == CE_Failure) {
            ERL_NIF_TERM reason = enif_make_string(env, CPLGetLastErrorMsg(), ERL_NIF_LATIN1);
            WARN("fail to write alpha to dsquery. errno=%T", reason);
            GDALClose(dsquery);
            return enif_raise_exception(env,
                    enif_make_tuple3(env,
                        enif_make_atom(env, "dsquery_alpha_write_error"),
                        enif_make_tuple2(env, enif_make_uint(env, tda->tx), enif_make_uint(env, tda->ty)),
                        reason));
        }
        //scale_query_to_tile(dsquery, dstile);
        for (uint32_t i = 1; i <= tda->tilebands; ++i) {
            GDALRasterBandH hDstBand = GDALGetRasterBand(dstile, i);
            if (CE_None != GDALRegenerateOverviews(GDALGetRasterBand(dsquery, i), 1, &hDstBand, "AVERAGE", NULL, NULL)) {
                ERL_NIF_TERM reason = enif_make_string(env, CPLGetLastErrorMsg(), ERL_NIF_LATIN1);
                GDALClose(dsquery);
                return enif_raise_exception(env,
                        enif_make_tuple3(env,
                            enif_make_atom(env, "scale_query_to_tile_error"),
                            enif_make_tuple2(env, enif_make_uint(env, tda->tx), enif_make_uint(env, tda->ty)),
                            reason));
            }
        }
        GDALClose(dsquery);
    }

    return ret;
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
            GDT_Byte,  // TODO: what about other type
            0,0);
    if (res != CE_None) {
        return enif_raise_exception(env,
                enif_make_string(env, "raster io error", ERL_NIF_LATIN1));
    }
    return enif_make_int(env, pixelValue);
}

static int nifload(ErlNifEnv* env, void ** __attribute__((unused)) priv_data, ERL_NIF_TERM  __attribute__((unused)) load_info) {
    GDALAllRegister();
    CPLSetErrorHandler(MyGDALErrorHandler);
    const char* gdalRuntimeReleaseName = GDALVersionInfo("GDAL_RELEASE_NAME");
    const char* gdalRuntimeVersionNum  = GDALVersionInfo("VERSION_NUM");
    DBG("GDAL release name %s", gdalRuntimeReleaseName);
    DBG("GDAL version num: %s", gdalRuntimeVersionNum);
    int res = GDALCheckVersion(2, 4, "WHAT the hell..., egdal2tiles");
    DBG("check 2.4 version: %d", res);
    res = GDALCheckVersion(3, 0, "what the hell...");
    DBG("check 3.0 version: %d", res);

    warpedDatasetResType = enif_open_resource_type(env, NULL, "warpedDataset", warped_dataset_dtor, ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER, NULL);
    tiledDatasetResType = enif_open_resource_type(env, NULL, "tiledDataset", tiled_dataset_dtor, ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER, NULL);
    assemblypartResType = enif_open_resource_type(env, NULL, "assemblypart", tiled_dataset_parts_dtor, ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER, NULL);

    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_TRUE = enif_make_atom(env, "true");
    ATOM_FALSE = enif_make_atom(env, "false");

    ATOM_PROFILE_MERCATOR = enif_make_atom(env, "mercator");
    ATOM_PROFILE_GEODETIC = enif_make_atom(env, "geodetic");

    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"is_warped",         1, is_warped,         0},
    {"create_vrt_copy",   1, create_vrt_copy,   0},
    {"has_nodata",        1, has_nodata,        0},
    {"open_with_profile", 2, open_with_profile, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"correct_dataset",   2, correct_dataset,   ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"get_xmlvrt",        1, get_xmlvrt,        ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"nb_data_bands",     1, nb_data_bands,     0},
    {"dataset_info",      1, dataset_info,      ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"band_info",         2, band_info,         ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"advise_read",       2, advise_read,       ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"extract_base_tile", 2, extract_base_tile, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"build_tile",        1, build_tile,        ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"write_png",         2, write_png,         ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"get_pixel",         3, get_pixel,         0}
};

ERL_NIF_INIT(gdalnif2tiles, nif_funcs, nifload, NULL,NULL,NULL)
