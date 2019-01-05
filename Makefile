PROJECT = gdalnif2tiles
PROJECT_DESCRIPTION = try 2tiles
PROJECT_VERSION = 0.1.0

CFLAGS += `pkg-config --cflags gdal`
LDLIBS += `pkg-config --libs gdal`

include erlang.mk
