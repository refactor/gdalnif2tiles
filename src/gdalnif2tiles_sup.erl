-module(gdalnif2tiles_sup).
-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-export([kickoff_tileworker/2]).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ?LOG_DEBUG("supervisor init..."),
    Procs = [#{id => gdal2tile_worker,
               start => {gdaltiles_worker, start_link, []},
               type => worker,
               modules => [gdaltiles_worker]
              }],
    {ok, {{simple_one_for_one, 10, 3600}, Procs}}.

kickoff_tileworker(Filename, Profile) ->
    ?LOG_DEBUG("kickoff for file: ~ts, with profile: ~p", [Filename, Profile]),
    supervisor:start_child(?MODULE, [Filename, Profile]).

