-module(gdal2tile_worker).
-behaviour(gen_statem).

%% API.
-export([start_link/2]).

%% gen_statem.
-export([callback_mode/0]).
-export([init/1]).
-export([create_base_tile/3]).
-export([store_base_tile/3]).
-export([handle_event/4]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {
          job_info :: world_profile:tile_job_info(),
          tiled_parts :: gdalnif2tiles:tiled_parts(),
          tile :: gdalnif2tiles:tiled_dataset()
}).

%% API.

-spec start_link(world_profile:tile_job_info(), world_profile:tile_detail()) -> {ok, pid()}.
start_link(JobInfo, TileDetail) ->
    gdalnif2tiles:advise_read(JobInfo, TileDetail),
    TilePart = gdalnif2tiles:extract_base_tile(JobInfo, TileDetail),
    gen_statem:start_link(?MODULE, {JobInfo,TilePart}, []).

%% gen_statem.

callback_mode() ->
    state_functions.

-define(HANDLE_COMMON, ?FUNCTION_NAME(T,C,D) -> handle_common(T,C,D)).

init({JobInfo,TilePart}) ->
    {ok, create_base_tile, #state{job_info = JobInfo, tiled_parts = TilePart}, [{next_event, internal, by_init}]}.

create_base_tile(internal, Msg, #state{job_info = JobInfo, tiled_parts = TilePart} = StateData) ->
    lager:debug("from msg: ~p, job_info: ~p", [Msg, JobInfo]),
    Tile = gdalnif2tiles:build_tile(TilePart),
    {next_state, store_base_tile, StateData#state{tile = Tile}, [{next_event, internal, by_create_basetile}]};
?HANDLE_COMMON.

store_base_tile(internal, EventData, #state{job_info = #{output_dir := Dir}, tile = Tile}) ->
    lager:info("STORE basetile to ~p by ~p, with event: ~p, tile: ~p", [Dir, ?MODULE, EventData, Tile]),
    gdalnif2tiles:write_png(Tile, Dir),
    stop;
?HANDLE_COMMON.

handle_common(EventType, Msg, StateData) ->
    lager:warning("unhandled event: ~p, msg: ~p, state: ~p", [EventType, Msg, StateData]),
    keep_state_and_data.

handle_event(_EventType, _EventData, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(Reason, StateName, _StateData) ->
    lager:info("terminate reason: ~p, from state: ~p", [Reason, StateName]),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
