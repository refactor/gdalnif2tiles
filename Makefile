PROJECT = gdalnif2tiles
PROJECT_DESCRIPTION = try 2tiles
PROJECT_VERSION = 0.1.0

CFLAGS += -std=c11 `pkg-config --cflags gdal`
LDLIBS += `pkg-config --libs gdal`

CSOURCE = $(wildcard $(C_SRC_DIR)/*.c)
D_FILES = $(CSOURCE:.c=.d)
CPPFLAGS += -MMD
-include $(D_FILES)

clean::
	-@rm -f $(wildcard $(C_SRC_DIR)/*.d)

include erlang.mk
