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
    [["gdal2tiles"], %% Cmd
     [{i,[]}, {o,[]}],          %% KeySpecs
     FlagSpecs,                 %% FlagSpecs
     fun kickoff/3
    ].


kickoff(CmdBase, Keys, [{profile, P}] = Flags) ->
    lager:debug("Cmd: ~p, Keys: ~p, profile => ~p", [CmdBase,Keys,P]),
    WP0 = maps:from_list(Flags),
    WP = case proplists:get_value(o, Keys) of
            undefined -> WP0;
            OD -> maps:put(output_dir, OD, WP0)
         end,

    case proplists:get_value(i, Keys) of
    undefined ->
        lager:debug("NO input image file"),
        Text = clique_status:text("NO input image file, set by i=file"),
        [clique_status:alert([Text])];
    F ->
        Profile = world_profile:init(WP),
        gdal2tiles_manager:kickoff_tileworkers(F, Profile),
        T0 = clique_status:text("                    gdal2tiles"),
        L1 = clique_status:table([maps:to_list(Profile)]),
        [T0, L1]
    end;

kickoff(CmdBase, Keys, Flags) ->
    lager:debug("Cmd: ~p, Keys: ~p, Flags => ~p", [CmdBase, Keys, Flags]),
    T0 = clique_status:text("----- kicKOff -----"),
    [T0].
