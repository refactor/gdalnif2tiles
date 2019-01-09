#pragma once
#include <gdal.h>

inline const char* rasterDataType(GDALDataType datatype) {
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
