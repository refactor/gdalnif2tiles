-module(gdal2tiles_console).
-export([command/1]).

-spec command([string()]) -> ok.
command(Cmd) ->
    lager:debug("cmd> ~p", [Cmd]),
    clique:run(Cmd).
