PROJECT = gdalnif2tiles
PROJECT_DESCRIPTION = try 2tiles
PROJECT_VERSION = 0.1.0

CFLAGS += -std=c11 `pkg-config --cflags gdal`
LDLIBS += `pkg-config --libs gdal`

CSOURCE = $(wildcard c_src/*.c)
D_FILES = $(CSOURCE:.c=.d)
CPPFLAGS += -MMD
-include $(D_FILES)

clean::
	-@rm -f $(wildcard $(C_SRC_DIR)/*.d)

SHELL_OPTS = -config config/sys.config

include erlang.mk
