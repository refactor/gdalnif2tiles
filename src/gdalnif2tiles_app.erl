-module(gdalnif2tiles_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	gdalnif2tiles_sup:start_link().

stop(_State) ->
	ok.
