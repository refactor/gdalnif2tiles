-module(gdal2tiles_cli_registry).

-export([register_cli/0]).

register_cli() ->
    lager:debug("register my cli...."),
    clique:register([gdal2tiles_cli]).

