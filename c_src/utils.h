#pragma once

#include <erl_nif.h>

#include <gdal.h>
#include <ogr_srs_api.h>
#include <gdalwarper.h>
#include <cpl_conv.h>

#include <math.h>
#include <inttypes.h>
#include <stdbool.h>

#include "mylog.h"

#define ARRLEN(arr) (sizeof(arr) / sizeof(arr[0]))

#define CHECK_RESULT(f) do {\
    CPLErr res = (f);\
     if (res != CE_None) {\
        ERL_NIF_TERM reason = enif_make_string(env, CPLGetLastErrorMsg(), ERL_NIF_LATIN1);\
        return enif_raise_exception(env, \
                enif_make_tuple2(env, enif_make_atom(env, #f), reason));\
     }\
} while(0)

#define CHECK_RESULT_AND_CLOSE(f, handle) do {\
    CPLErr res = (f);\
     if (res != CE_None) {\
        ERL_NIF_TERM reason = enif_make_string(env, CPLGetLastErrorMsg(), ERL_NIF_LATIN1);\
        GDALClose(handle);\
        return enif_raise_exception(env, \
                enif_make_tuple2(env, enif_make_atom(env, #f), reason));\
     }\
} while(0)

#define CHECK_RESULT_AND_FREE(f, tgt) do {\
    CPLErr res = (f);\
     if (res != CE_None) {\
        ERL_NIF_TERM reason = enif_make_string(env, CPLGetLastErrorMsg(), ERL_NIF_LATIN1);\
        VSIFree(tgt);\
        tgt = NULL;\
        return enif_raise_exception(env, \
                enif_make_tuple2(env, enif_make_atom(env, #f), reason));\
     }\
} while(0)


/*
 * The no data value for a band is generally a special marker value used to mark pixels that are not valid data.
 * Such pixels should generally not be displayed, nor contribute to analysis operations.
 */
typedef struct nodata_list {
    uint32_t bandCount;
    double nodata[];
} nodata_list;

/**
 * orginal GDAL dataset
 */
/**
 * warped GDAL dataset: for reprojection & some fix for nodatavalue
 *   reproject according world-profile: MERCATOR or GEODETIC
 */
typedef struct WarpedDataset {
    GDALDatasetH raw_input_dataset;
    GDALDatasetH warped_input_dataset;

    OGRSpatialReferenceH output_srs;

    char vmfilename[128];
    nodata_list *nodata;
} WarpedDataset;

static void cat_novalues(const nodata_list* nodata, char buf[], size_t buf_sz) {
    char *ptr = buf;
    int n = snprintf(ptr, buf_sz, "%f", nodata->nodata[0]);
    for (uint32_t i = 1; i < nodata->bandCount; ++i) {
        buf_sz -= n;
        ptr += n;
        n = snprintf(ptr, buf_sz, " %f", nodata->nodata[i]);
    }
}

static inline GDALDatasetH reprojectDataset(const GDALDatasetH hSrcDS, OGRSpatialReferenceH dstSRS) {
    GDALDatasetH hDstDS = NULL;

    const char* srcSRSWKT = GDALGetProjectionRef(hSrcDS);
    OGRSpatialReferenceH srcSRS = OSRNewSpatialReference(NULL);
    if (srcSRSWKT == NULL || OGRERR_NONE != OSRSetFromUserInput(srcSRS, srcSRSWKT)) {
    //    GDALClose(hDataset);
        OSRDestroySpatialReference(srcSRS);
        return NULL;
    }
    if (OSRIsSame(srcSRS, dstSRS)) {
        hDstDS = hSrcDS;
    }
    else {
        char *pszDstSRSWKT = NULL;
        OSRExportToWkt(dstSRS, &pszDstSRSWKT);
        DBG("Warping of the raster by AutoCreateWarpedVRT (result saved into 'tiles.vrt' for verbose), the work are lengthy");
        hDstDS = GDALAutoCreateWarpedVRT(hSrcDS,
                                         srcSRSWKT, pszDstSRSWKT,
                                         GRA_NearestNeighbour,
                                         0.0,
                                         NULL);
        //hDstDS.GetDriver().CreateCopy("tiles.vrt", to_dataset)
        CPLFree(pszDstSRSWKT);
    }
    OSRDestroySpatialReference(srcSRS);
    return hDstDS;
}

