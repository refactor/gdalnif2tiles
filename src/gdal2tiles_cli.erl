-module(gdal2tiles_cli).

-behaviour(clique_handler).

-export([register_cli/0]).
-export([kickoff/3]).


register_cli() ->
    clique:register_usage(["gdal2tiles", "kickoff"], cmd_usage()),
    apply(clique, register_command, kickoff_register()).

cmd_usage() ->
    [
     "gdal2tiles <sub-command>\n\n",
     "  Display status and setting.\n\n",
     "  Sub-comands:\n",
     "    status    Display current status\n",
     "    kickoff   Do the work\n\n",
     "  Use --help after a sub-command for more details.\n"
    ].

kickoff_register() ->
    [["gdal2tiles", "kickoff"], %% Cmd
     [],                        %% KeySpecs
     [],                        %% FlagSpecs
     fun kickoff/3
    ].


kickoff(_CmdBase, [], []) ->
    T0 = clique_status:text("----- kickoff -----"),
    [T0].
