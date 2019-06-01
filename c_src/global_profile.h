#pragma once

#include <math.h>
#include <stdint.h>
#include <stdbool.h>

#include <gdal.h>
#include <ogr_srs_api.h>
#include <erl_nif.h>
#include "mylog.h"

/**
 * global profile for TMS: mercator OR geodetic
 *
 * profile: used for geospatial projection parameters and functions, such as
 *  1) resolution
 *  2) pixel...
 *  3) transfer
 *
 */

#define MAXZOOMLEVEL 32

typedef enum profile_type {
    MERCATOR = 0,
    GEODETIC = 1,
    PROFILE_TYPE_COUNT
} profile_type;

#define KN(F) [F] = #F
const char* WORLD_PROFILE_TYPES[] = {KN(MERCATOR), KN(GEODETIC)};
#undef KN

typedef struct WorldProfile {
    profile_type type;
    int32_t tileSize;
    union {
        double initialResolution;
        double resFact;
    };
    double originShift;
    bool tmscompatible; // false as default
    OGRSpatialReferenceH output_srs;
} WorldProfile;

void initProfile(const profile_type profileType, WorldProfile *profile) {
    int tileSize = 256;
    double initResolution = 0;
    double originShift = 0;
    OGRSpatialReferenceH output_srs = OSRNewSpatialReference(NULL);
    //if (OSRSetAxisMappingStrategy) OSRSetAxisMappingStrategy(output_srs, OAMS_TRADITIONAL_GIS_ORDER);
    switch(profileType) {
        case MERCATOR:
            OSRImportFromEPSG(output_srs, 3857);
            initResolution = 2 * M_PI * 6378137 / tileSize;
            originShift = 2 * M_PI * 6378137 / 2.0;
            profile->initialResolution = initResolution;
            break;
        case GEODETIC:
            OSRImportFromEPSG(output_srs, 4326);
            if (profile->tmscompatible)
                initResolution = 180.0 / tileSize;  // OSGeo TMS spec: 2 tiles @ level 0
            else
                initResolution = 360.0 / tileSize;  // OpenLayers, WMTS: 1 tiles @ level 0
            profile->resFact = initResolution;
            break;
        default:
            WARN("unknow profile: %d", profileType);
            return;
    }
    profile->tileSize = tileSize;
    profile->output_srs = output_srs;
    profile->originShift = originShift;
    profile->type = profileType;
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
    switch (profile->type) {
    case MERCATOR: {
            double res = resolutionOf(profile->initialResolution, zoom);
            LOG("res: %f, originShift: %f", res, profile->originShift);
            bounds[0] = pixelToMeter(profile, res, tx * profile->tileSize);
            bounds[1] = pixelToMeter(profile, res, ty * profile->tileSize);
            bounds[2] = pixelToMeter(profile, res, (tx + 1) * profile->tileSize);
            bounds[3] = pixelToMeter(profile, res, (ty + 1) * profile->tileSize);
        }
        break;
    case GEODETIC: {
            double res = resolutionOf(profile->resFact, zoom);
            bounds[0] = tx * profile->tileSize * res - 180.0;
            bounds[1] = ty * profile->tileSize * res - 90.0;
            bounds[2] = (tx + 1) * profile->tileSize * res - 180.0;
            bounds[3] = (ty + 1) * profile->tileSize * res - 90.0;
        }
        break;
    default:
        WARN("unknown profile");
    }
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

