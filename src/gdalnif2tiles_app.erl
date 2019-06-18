-module(gdalnif2tiles_app).
-behaviour(application).

-export([start/0]).

-export([start/2]).
-export([stop/1]).

start() ->
    application:ensure_all_started(gdalnif2tiles).

start(_Type, _Args) ->
	case gdalnif2tiles_sup:start_link() of
        {ok, Pid} ->
            gdal2tiles_cli_registry:register_cli(),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
	ok.
