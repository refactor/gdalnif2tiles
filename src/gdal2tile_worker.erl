-module(gdal2tile_worker).
-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").

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
          tile_detail :: world_profile:tile_detail(),
          tile :: gdalnif2tiles:tiled_dataset()
}).

%% API.

-spec start_link(world_profile:tile_job_info(), world_profile:tile_detail()) -> {ok, pid()}.
start_link(JobInfo, TileDetail) ->
    gdalnif2tiles:advise_read(JobInfo, TileDetail),
    gen_statem:start_link(?MODULE, {JobInfo,TileDetail}, []).

%% gen_statem.

callback_mode() ->
    state_functions.

-define(HANDLE_COMMON, ?FUNCTION_NAME(T,C,D) -> handle_common(T,C,D)).

init({JobInfo,TileDetail}) ->
    {ok, create_base_tile, #state{job_info = JobInfo, tile_detail = TileDetail}, [{next_event, internal, by_init}]}.

create_base_tile(internal, Msg, #state{job_info = JobInfo, tile_detail = D} = StateData) ->
    ?LOG_DEBUG("from msg: ~p, job_info: ~p, tile_detail: ~p", [Msg, JobInfo, D]),
    Tile = gdalnif2tiles:create_base_tile(JobInfo, D),
    {next_state, store_base_tile, StateData#state{tile = Tile}, [{next_event, internal, by_create_basetile}]};
?HANDLE_COMMON.

store_base_tile(internal, EventData, #state{job_info = #{output_dir := Dir}, tile = Tile}) ->
    ?LOG_INFO("STORE basetile to ~p by ~p, with event: ~p, tile: ~p", [Dir, ?MODULE, EventData, Tile]),
    gdalnif2tiles:write_png(Tile, "/tmp/"),
    stop;
?HANDLE_COMMON.

handle_common(EventType, Msg, StateData) ->
    ?LOG_WARNING("unhandled event: ~p, msg: ~p, state: ~p", [EventType, Msg, StateData]),
    keep_state_and_data.

handle_event(_EventType, _EventData, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(Reason, StateName, _StateData) ->
    ?LOG_INFO("terminate reason: ~p, from state: ~p", [Reason, StateName]),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
