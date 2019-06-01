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
    bool warped;    // tag to determine GC
    GDALDatasetH warped_input_dataset;

    OGRSpatialReferenceH output_srs;

    char vmfilename[128];
    nodata_list *nodata;
} WarpedDataset;

static void cat_novalues(const nodata_list* nodata, char buf[], size_t buf_sz) {
    char *ptr = buf;
    int n = snprintf(ptr, buf_sz, "%f", nodata->nodata[0]);
    for (int i = 1; i < nodata->bandCount; ++i) {
        buf_sz -= n;
        ptr += n;
        n = snprintf(ptr, buf_sz, " %f", nodata->nodata[i]);
    }
}

static inline GDALDatasetH reprojectDataset(const GDALDatasetH ds, OGRSpatialReferenceH dstSRS) {
    GDALDatasetH hSrcDS = ds;
    GDALDatasetH hDstDS = NULL;

    const char* proj = GDALGetProjectionRef(ds);
    OGRSpatialReferenceH fileSRS = OSRNewSpatialReference(NULL);
    if (proj == NULL || OGRERR_NONE != OSRSetFromUserInput(fileSRS, proj)) {
    //    GDALClose(hDataset);
        OSRDestroySpatialReference(fileSRS);
        return NULL;
    }
    if (OSRIsSame(fileSRS, dstSRS)) {
        hDstDS = ds;
    }
    else {
        char *pszDstWKT = NULL;
        OSRExportToWkt(dstSRS, &pszDstWKT);
        LOG("Warping of the raster by AutoCreateWarpedVRT (result saved into 'tiles.vrt' for verbose)");
        hDstDS = GDALAutoCreateWarpedVRT(hSrcDS,
                                         GDALGetProjectionRef(hSrcDS), pszDstWKT,
                                         GRA_NearestNeighbour,
                                         0.0,
                                         NULL);
        //hDstDS.GetDriver().CreateCopy("tiles.vrt", to_dataset)
        CPLFree(pszDstWKT);
    }
    return hDstDS;
}

static inline const char* rasterDataType(GDALDataType datatype) {
    switch (datatype) {
        case GDT_Float32:
            return "float32";
        case GDT_Float64:
            return "float64";
        case GDT_Byte:
            return "byte";
        case GDT_UInt16:
            return "uint16";
        case GDT_Int16:
            return "int16";
        case GDT_UInt32:
            return "uint32";
        case GDT_Int32:
            return "int32";
        case GDT_Unknown:
            return "unknow";
        default:
            return "complex ignored";
    }
}

