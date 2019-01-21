#pragma once

#include <gdal.h>
#include <ogr_srs_api.h>

#include <math.h>
#include <inttypes.h>

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
