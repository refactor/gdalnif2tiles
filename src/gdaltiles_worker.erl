-module(gdaltiles_worker).
-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").

%% API.
-export([start_link/2]).

%% gen_server.
-export([callback_mode/0]).
-export([init/1]).
-export([generate_base_tiles/3]).
-export([create_base_tiles/3]).
-export([write_to_png/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(TIMEOUT, 5000).

-record(state, {
          profile :: global_profile:profile(),
          gdal2tiles :: map(),
          job_info :: global_profile:tile_job_info(),
          details :: [global_profile:tile_detail()],
          tiles :: [{gdalnif2tils:tiled_dataset(), global_profile:tile_detail()}]
}).

%% API.

-spec start_link(file:filename(), global_profile:profile()) -> {ok, pid()}.
start_link(Filename, Profile) ->
    ?LOG_DEBUG("~p:start_link(~p, ~p)...", [?MODULE, Filename, Profile]),
%    gen_server:start_link(?MODULE, [Filename, Profile], []).
    {ok, proc_lib:spawn_link(?MODULE, init, [{Filename, Profile}])}.

%% gen_statem.

callback_mode() ->
    state_functions.

init({Filename, Profile}) ->
    ?LOG_DEBUG("~p init for file: ~p, with profile: ~p", [?MODULE, Filename, Profile]),
    WS = gdalnif2tiles:open_to(Filename, Profile),
    RasterInfo = gdalnif2tiles:info(WS),
    self() ! to_generate,
    gen_statem:enter_loop(?MODULE, [], generate_base_tiles, #state{profile = Profile, gdal2tiles = RasterInfo}).

generate_base_tiles(EventType, _Msg, #state{profile = Profile, gdal2tiles = RasterInfo} = StateData) ->
    ?LOG_DEBUG("generate_base_tiles... ~p", [EventType]),
    {JobInfo, Details} = global_profile:generate_base_tiles(Profile, RasterInfo),
    self() ! to_crate_base_tiles,
    {next_state, create_base_tiles, StateData#state{job_info = JobInfo, details = Details}}.

create_base_tiles(EventType, _Msg, #state{job_info = JobInfo, details = Details} = StateData) ->
    ?LOG_DEBUG("create_base_tiles...", [EventType]),
    Tiles = lists:map(fun(D) -> Tile = gdalnif2tiles:create_base_tile(JobInfo, D), {Tile, D} end, Details),
    %%Tiles = [ gdalnif2tiles:create_base_tile(JobInfo, D) || D <- Details],
    {next_state, write_to_png, StateData#state{tiles = Tiles}}.

write_to_png(EventType, _Msg, #state{tiles = Tiles} = _StateData) ->
    ?LOG_DEBUG("write_to_png...~p, with: ~p",  [EventType, length(Tiles)]),
    keep_state_and_data.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
