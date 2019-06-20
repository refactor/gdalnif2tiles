-module(gdal2tiles_cli).

-behaviour(clique_handler).

-export([register_cli/0]).
-export([kickoff/3]).


register_cli() ->
    clique:register_usage(["gdal2tiles"], cmd_usage()),
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
    FlagSpecs = [{profile,[{shortname, "p"}, {longname, "profile"},
                           {typecast, fun list_to_atom/1}
                          ]}],
    [["gdal2tiles", "kickoff"], %% Cmd
     [],                        %% KeySpecs
     FlagSpecs,                 %% FlagSpecs
     fun kickoff/3
    ].


kickoff(CmdBase, Keys, [{profile, P}] = _Flags) ->
    lager:debug("Cmd: ~p, Keys: ~p, profile => ~p", [CmdBase,Keys,P]),
    T0 = clique_status:text("----- kickoff -----"),
    [T0];
kickoff(CmdBase, Keys, Flags) ->
    lager:debug("Cmd: ~p, Keys: ~p, Flags => ~p", [CmdBase, Keys, Flags]),
    T0 = clique_status:text("----- kickoff -----"),
    [T0].
