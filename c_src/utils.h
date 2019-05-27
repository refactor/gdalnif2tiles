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

#define MAXZOOMLEVEL 32

typedef enum world_profile_type {
    MERCATOR = 0,
    GEODETIC = 1,
    PROFILE_TYPE_COUNT
} world_profile_type;

#define KN(F) [F] = #F
const char* WORLD_PROFILE_TYPES[] = {KN(MERCATOR), KN(GEODETIC)};
#undef KN

typedef struct WorldProfile {
    int tileSize;
    union {
        double initialResolution;
        double resFact;
    };
    double originShift;
    OGRSpatialReferenceH outputSRS;
} WorldProfile;

/*
 * The no data value for a band is generally a special marker value used to mark pixels that are not valid data.
 * Such pixels should generally not be displayed, nor contribute to analysis operations.
 */
typedef struct nodata_list {
    uint32_t len;
    double nodata[];
} nodata_list;

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

    // @depracated
    const WorldProfile *profile;
} MyGDALDataset;

typedef struct WarpedDataset {
    bool warped;
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

void createProfile(world_profile_type profileType, WorldProfile *profile) {
    int tileSize = 256;
    double initResolution = 0;
    double originShift = 0;
    OGRSpatialReferenceH output_srs = OSRNewSpatialReference(NULL);
    switch(profileType) {
        case MERCATOR:
            initResolution = 2 * M_PI * 6378137 / tileSize;
            originShift = 2 * M_PI * 6378137 / 2.0;
            OSRImportFromEPSG(output_srs, 3857);
            profile->tileSize = tileSize;
            profile->initialResolution = initResolution;
            profile->outputSRS = output_srs;
            profile->originShift = originShift;
            return;
        case GEODETIC:
            initResolution = 180.0 / tileSize;
            OSRImportFromEPSG(output_srs, 4326);
            profile->tileSize = tileSize;
            profile->resFact = initResolution;
            profile->outputSRS = output_srs;
            profile->originShift = originShift;
            return;
        default:
            return;
    }
}

static inline double resolutionOf(const double initialResolution, int zoom) {
    return initialResolution / pow(2, zoom);
}

static inline int zoomForPixelSize(const WorldProfile *profile, double pixelSize) {
    for (int i=0; i<MAXZOOMLEVEL; ++i) {
        if (pixelSize > resolutionOf(profile->initialResolution, i)) {
            return MAX(0, i - 1); // from cpl_port.h
        }
    }
    return MAXZOOMLEVEL - 1;
}

static inline double pixelToMeter(const WorldProfile * profile, const double resolution, double xy) {
    return xy * resolution - profile->originShift;
}

void tileBounds(const WorldProfile * profile, const int tx, const int ty, const int zoom, double bounds[static 4]) {
    double res = resolutionOf(profile->initialResolution, zoom);
    LOG("res: %f, originShift: %f", res, profile->originShift);
    bounds[0] = pixelToMeter(profile, res, tx * profile->tileSize);
    bounds[1] = pixelToMeter(profile, res, ty * profile->tileSize);
    bounds[2] = pixelToMeter(profile, res, (tx + 1) * profile->tileSize);
    bounds[3] = pixelToMeter(profile, res, (ty + 1) * profile->tileSize);
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

