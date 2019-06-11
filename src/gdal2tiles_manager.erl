-module(gdal2tiles_manager).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

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

-define(TIMEOUT, 30000).

-record(state, {
          profile :: global_profile:profile(),
          imgfile :: file:filename(),
          gdal2tiles :: map(),
          job_info :: global_profile:tile_job_info(),
          details :: [global_profile:tile_detail()],
          tiles = [] :: [{gdalnif2tils:tiled_dataset(), global_profile:tile_detail()}]
}).

%% API.

-spec kickoff_tileworkers(file:filename(), global_profile:profile()) -> {ok, pid()}.
kickoff_tileworkers(Filename, Profile) ->
    ?LOG_INFO("~p:start_link(~p, ~p)...", [?MODULE, Filename, Profile]),
    gen_server:call(?MODULE, {kickoff_tileworkers, Filename, Profile}, ?TIMEOUT).
%    {ok, proc_lib:spawn_link(?MODULE, init, [{Filename, Profile}])}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({kickoff_tileworkers, Filename, Profile}, _From, State) ->
    ?LOG_INFO("~p kickoff for file: ~p, with profile: ~p", [?MODULE, Filename, Profile]),
    WS = gdalnif2tiles:open_to(Filename, Profile),
    ?LOG_INFO("opened"),
    RasterInfo = gdalnif2tiles:info(WS),
    ?LOG_INFO("infoed"),
    {JobInfo, Details} = global_profile:generate_base_tiles(Profile, RasterInfo),
    lists:foreach(fun(D) -> gdal2tile_worker_sup:kickoff_tileworker(JobInfo, D) end, Details),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ?LOG_INFO("terminate, reason: ~p, froM state: ~p", [Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
