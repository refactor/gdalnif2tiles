-module(gdal2tiles_manager).
-behaviour(gen_server).

%% API.
-export([kickoff_tileworkers/2]).
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(TIMEOUT, infinity).

-record(state, {
          profile :: world_profile:profile(),
          imgfile :: file:filename(),
          gdal2tiles :: map(),
          job_info :: world_profile:tile_job_info(),
          details :: [world_profile:tile_detail()],
          tiles = [] :: [{gdalnif2tils:tiled_dataset(), world_profile:tile_detail()}]
}).

%% API.

-spec kickoff_tileworkers(file:filename(), world_profile:profile()) -> {ok, pid()}.
kickoff_tileworkers(Filename, Profile) ->
    lager:info("~p:start_link(~p, ~p)...", [?MODULE, Filename, Profile]),
    gen_server:call(?MODULE, {kickoff_tileworkers, Filename, Profile}, ?TIMEOUT).
%    {ok, proc_lib:spawn_link(?MODULE, init, [{Filename, Profile}])}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({kickoff_tileworkers, Filename, Profile}, _From, State) ->
    lager:info("~p kickoff for file: ~p, with profile: ~p", [?MODULE, Filename, Profile]),
    WS = gdalnif2tiles:open_to(Filename, Profile),
    lager:info("opened"),
    RasterInfo = gdalnif2tiles:info(WS),
    lager:info("infoed"),
    {JobInfo, Details} = world_profile:generate_base_tiles(Profile, RasterInfo),
    lists:foreach(fun(D) -> gdal2tile_worker_sup:kickoff_tileworker(JobInfo, D) end, Details),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    lager:info("terminate, reason: ~p, froM state: ~p", [Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
