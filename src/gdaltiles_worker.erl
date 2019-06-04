-module(gdaltiles_worker).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API.
-export([start_link/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(TIMEOUT, 5000).

-record(state, {
          gdal2tiles :: map()
}).

%% API.

-spec start_link(file:filename(), global_profile:profile()) -> {ok, pid()}.
start_link(Filename, Profile) ->
    ?LOG_DEBUG("~p:start_link(~p, ~p)...", [?MODULE, Filename, Profile]),
%    gen_server:start_link(?MODULE, [Filename, Profile], []).
    {ok, proc_lib:spawn_link(?MODULE, init, [{Filename, Profile}])}.

%% gen_server.

init({Filename, Profile}) ->
    ?LOG_DEBUG("~p init for file: ~p, with profile: ~p", [?MODULE, Filename, Profile]),
    WS = gdalnif2tiles:open_to(Filename, Profile),
    RasterInfo = gdalnif2tiles:info(WS),
    gen_server:enter_loop(?MODULE, [], #state{gdal2tiles = RasterInfo}, ?TIMEOUT).

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
