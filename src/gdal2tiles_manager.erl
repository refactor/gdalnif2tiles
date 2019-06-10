-module(gdal2tiles_manager).
-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").

%% API.
-export([start_link/2]).
-export([stop/1]).

%% gen_server.
-export([callback_mode/0]).
-export([init/1]).
-export([generate_base_tiles/3]).
-export([create_base_tiles/3]).
-export([write_to_png/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/3]).
-export([code_change/4]).

-define(TIMEOUT, 5000).

-record(state, {
          profile :: global_profile:profile(),
          imgfile :: file:filename(),
          gdal2tiles :: map(),
          job_info :: global_profile:tile_job_info(),
          details :: [global_profile:tile_detail()],
          tiles :: [{gdalnif2tils:tiled_dataset(), global_profile:tile_detail()}]
}).

%% API.

-spec start_link(file:filename(), global_profile:profile()) -> {ok, pid()}.
start_link(Filename, Profile) ->
    ?LOG_DEBUG("~p:start_link(~p, ~p)...", [?MODULE, Filename, Profile]),
    gen_statem:start_link(?MODULE, {Filename, Profile}, []).
%    {ok, proc_lib:spawn_link(?MODULE, init, [{Filename, Profile}])}.

stop(Pid) ->
    gen_statem:stop(Pid).

%% gen_statem.

-define(HANDLE_COMMON, ?FUNCTION_NAME(T,C,D) -> handle_common(T,C,D)).

callback_mode() ->
    [state_functions, state_enter].

init({Filename, Profile}) ->
    ?LOG_DEBUG("~p init for file: ~p, with profile: ~p", [?MODULE, Filename, Profile]),
    process_flag(trap_exit, true),
    {ok, generate_base_tiles, #state{imgfile = Filename, profile = Profile}, [{next_event, internal, generate_Basetiles}]}.
%    gen_statem:enter_loop(?MODULE, [], generate_base_tiles, #state{profile = Profile, gdal2tiles = RasterInfo}).

generate_base_tiles(enter, Msg, #state{imgfile = Filename, profile = Profile} = StateData) ->
    ?LOG_DEBUG("generate_base_tiles entering FROM... ~p", [Msg]),
    WS = gdalnif2tiles:open_to(Filename, Profile),
    RasterInfo = gdalnif2tiles:info(WS),
    {keep_state, StateData#state{gdal2tiles = RasterInfo}};
generate_base_tiles(internal, Msg, #state{profile = Profile, gdal2tiles = RasterInfo} = StateData) ->
    ?LOG_DEBUG("generate_base_tiles... ~p", [Msg]),
    {JobInfo, Details} = global_profile:generate_base_tiles(Profile, RasterInfo),
    {next_state, create_base_tiles, StateData#state{job_info = JobInfo, details = Details}, [{next_event, internal, generatE_basetiles}]};
?HANDLE_COMMON.

create_base_tiles(enter, Msg, StateData) ->
    ?LOG_DEBUG("create_base_tiles entering FROM... ~p", [Msg]),
    {keep_state, StateData};
create_base_tiles(internal, Msg, #state{details = [], tiles = Tiles} = StateData) ->
    ?LOG_DEBUG("DONE create_base_tiles... with msg: ~p", [Msg]),
    {next_state, write_to_png, StateData#state{tiles = Tiles}};
create_base_tiles(internal, Msg, #state{job_info = JobInfo, details = [D | RestDetails], tiles = Tiles} = StateData) ->
    ?LOG_DEBUG("create_base_tiles... with msg: ~p", [Msg]),
%    Tiles = lists:map(fun(D) -> Tile = gdalnif2tiles:create_base_tile(JobInfo, D), {Tile, D} end, Details),
    Tile = gdalnif2tiles:create_base_tile(JobInfo, D),
    {keep_state, StateData#state{details = RestDetails, tiles = [{Tile,D} | Tiles]}, [{next_event, internal, create_basetile}]};
?HANDLE_COMMON.

write_to_png(enter, Msg, _StateData) ->
    ?LOG_DEBUG("Enter FROM ~p", [Msg]),
    keep_state_and_data;
write_to_png(EventType, _Msg, #state{tiles = Tiles} = _StateData) ->
    ?LOG_DEBUG("write_to_png...~p, with: ~p",  [EventType, length(Tiles)]),
    stop;
?HANDLE_COMMON.

handle_common(EventType, Msg, StateData) ->
    ?LOG_WARNING("unhandled event: ~p, msg: ~p, state: ~p", [EventType, Msg, StateData]),
    keep_state_and_data.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State, Data) ->
    ?LOG_DEBUG("terminate, reason: ~p, state: ~p, data: ~p", [Reason, State, Data]),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.
