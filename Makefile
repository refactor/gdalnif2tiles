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

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'
ERLC_OPTS += +'{lager_truncation_size, 1024}'

DEPS = lager clique recon 
dep_lager_commit = 3.7.0
dep_clique_commit = develop-3.0
dep_recon_commit = 2.5.0

clean::
	-@rm -f $(wildcard $(C_SRC_DIR)/*.d)

CONFIG ?= config/sys.config
EXTRA_CONFIG ?= config/extra.config

SHELL=/bin/bash
SHELL_OPTS = -s gdalnif2tiles_app -config ${CONFIG} -args_file config/vm.args

shell::
	@if ! [[ -a ${EXTRA_CONFIG} ]]; then echo "[]." > ${EXTRA_CONFIG}; fi

include erlang.mk
