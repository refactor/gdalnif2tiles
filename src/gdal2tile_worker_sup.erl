-module(gdal2tile_worker_sup).
-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-export([kickoff_tileworker/2]).
-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ?LOG_DEBUG("workers supervisor init..."),
    process_flag(trap_exit, true),
    Procs = [#{id => gdal2tiles_manager,
               start => {gdal2tile_worker, start_link, []},
               type => worker,
               restart => transient,
               modules => [gdal2tile_worker_sup, gdal2tile_worker]
              }],
    SupFlags = #{strategy => simple_one_for_one, intensity => 10, period => 3600},
    {ok, {SupFlags, Procs}}.

kickoff_tileworker(JobInfo, D) ->
    supervisor:start_child(?MODULE, [JobInfo, D]).

