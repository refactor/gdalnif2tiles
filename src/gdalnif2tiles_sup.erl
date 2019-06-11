-module(gdalnif2tiles_sup).
-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ?LOG_DEBUG("app supervisor init..."),
    Procs = [#{id => gdal2tiles_manager,
               start => {gdal2tiles_manager, start_link, []},
               type => worker,
               modules => [gdal2tiles_manager]
              }, 
             #{id => gdal2tile_worker_sup,
               start => {gdal2tile_worker_sup, start_link, []},
               type => supervisor,
               modules => [gdal2tile_worker_sup, gdal2tile_worker]
              }],
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    {ok, {SupFlags, Procs}}.

