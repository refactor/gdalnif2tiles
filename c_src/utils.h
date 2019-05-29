#pragma once

#include <erl_nif.h>

#include <gdal.h>
#include <ogr_srs_api.h>
#include <gdalwarper.h>
#include <cpl_conv.h>

#include <math.h>
#include <inttypes.h>
#include <stdbool.h>

#include "global_profile.h"

#include "mylog.h"

/*
 * The no data value for a band is generally a special marker value used to mark pixels that are not valid data.
 * Such pixels should generally not be displayed, nor contribute to analysis operations.
 */
typedef struct nodata_list {
    uint32_t len;
    double nodata[];
} nodata_list;

/**
 * orginal GDAL dataset
 */
typedef struct MyGDALDataset {
    GDALDatasetH handle;
    OGRSpatialReferenceH inputSRS;
    int srid;
    int rasterCount;
    int rasterWidth, rasterHeight;
    double originX, originY;
    double pixelWidth, pixelHeight;
    double minBoundX, maxBoundX;
    double minBoundY, maxBoundY;
    double minBoundZ, maxBoundZ;
    const nodata_list *in_nodata;
} MyGDALDataset;

/**
 * warped GDAL dataset: for reprojection & some fix for nodatavalue
 *   reproject according world-profile: MERCATOR or GEODETIC
 */
typedef struct WarpedDataset {
    bool warped;    // tag to determine GC
    GDALDatasetH warped_input_dataset;

    VSILFILE* memFile;
    const WorldProfile *profile;
    const MyGDALDataset* myGDALDataset;
} WarpedDataset;

static void cat_novalues(const nodata_list* nodata, char buf[], size_t buf_sz) {
    char *ptr = buf;
    int n = snprintf(ptr, buf_sz, "%f", nodata->nodata[0]);
    for (int i = 1; i < nodata->len; ++i) {
        buf_sz -= n;
        ptr += n;
        n = snprintf(ptr, buf_sz, " %f", nodata->nodata[i]);
    }
}

static inline GDALDatasetH reprojectDataset(const MyGDALDataset* pGDALDataset, OGRSpatialReferenceH dstSRS) {
    GDALDatasetH hSrcDS = pGDALDataset->handle;
    GDALDatasetH hDstDS = NULL;
    if (OSRIsSame(pGDALDataset->inputSRS, dstSRS)) {
        hDstDS = pGDALDataset->handle;
    }
    else {
        char *pszDstWKT = NULL;
        OSRExportToWkt(dstSRS, &pszDstWKT);

        hDstDS = GDALAutoCreateWarpedVRT(hSrcDS,
                                         GDALGetProjectionRef(hSrcDS), pszDstWKT,
                                         GRA_NearestNeighbour,
                                         0.0,
                                         NULL);
        CPLFree(pszDstWKT);
    }
    return hDstDS;
}

