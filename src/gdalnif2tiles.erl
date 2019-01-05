-module(gdalnif2tiles).
-export([open_file/1]).
-export([info/1]).
-export([band_info/2]).

-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                {error,_} ->
                      AppPath = filename:dirname(filename:dirname(code:which(?MODULE))),
                      filename:join(AppPath, "priv");
                Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, "gdalnif2tiles"), 0).

-spec open_file(file:filename()) -> reference().
open_file(_File) ->
    erlang:nif_error(notfound).

-spec info(reference()) -> map().
info(_Dataset) ->
    erlang:nif_error(notfound).

-spec band_info(reference(), pos_integer()) -> map().
band_info(_Dataset, _BandNo) ->
    erlang:nif_error(notfound).
