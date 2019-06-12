PROJECT = gdalnif2tiles
PROJECT_DESCRIPTION = try 2tiles
PROJECT_VERSION = 0.1.0

CFLAGS += -std=c11 `pkg-config --cflags gdal`
CFLAGS +=-DMYDEBUG
LDLIBS += `pkg-config --libs gdal`

CSOURCE = $(wildcard c_src/*.c)
D_FILES = $(CSOURCE:.c=.d)
CPPFLAGS += -MMD
-include $(D_FILES)

clean::
	-@rm -f $(wildcard $(C_SRC_DIR)/*.d)

CONFIG ?= config/sys.config
EXTRA_CONFIG ?= config/extra.config

SHELL=/bin/bash
SHELL_OPTS = -config ${CONFIG} -s gdalnif2tiles_app

shell::
	@if ! [[ -a ${EXTRA_CONFIG} ]]; then echo "[]." > ${EXTRA_CONFIG}; fi

include erlang.mk
