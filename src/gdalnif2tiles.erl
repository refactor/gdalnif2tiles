-module(gdalnif2tiles).
-export([create_profile/1]).
-export([open_file/1]).
-export([info/1]).
-export([band_info/2]).
-export([get_pixel/3]).
-export([update_no_data_values/1]).

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

-spec create_profile('MERCATOR'|'GEODETIC') -> reference().
create_profile(_Profile) ->
    erlang:nif_error(notfound).

-spec open_file(file:filename()) -> reference().
open_file(_File) ->
    erlang:nif_error(notfound).

-spec info(reference()) -> map().
info(_Dataset) ->
    erlang:nif_error(notfound).

-spec band_info(reference(), pos_integer()) -> map().
band_info(_Dataset, _BandNo) ->
    erlang:nif_error(notfound).

-spec get_pixel(reference(), non_neg_integer(), non_neg_integer()) -> list(float()).
get_pixel(_Dataset, _X, _Y) ->
    erlang:nif_error(notfound).

update_no_data_values(File) ->
    {XmlDoc,_} = xmerl_scan:file(File),
    TL = xmerl_lib:simplify_element(XmlDoc),
    NewTL = add_gdal_warp_options_to_string(TL),
    % rm header: "<?xml version=\"1.0\"?>"
    lists:flatten(tl(xmerl:export_simple([NewTL], xmerl_xml))).

add_gdal_warp_options_to_string({Tag, Attributes, Content}) ->
    NewContent = add_gdal_warp_options(Content),
    {Tag, Attributes, NewContent}.

add_gdal_warp_options([{'GDALWarpOptions',Attrs,Content} | Rest]) ->
    [{'GDALWarpOptions',Attrs, add_option(Content)} | Rest];
add_gdal_warp_options([Head|Rest]) ->
    [Head | add_gdal_warp_options(Rest)].

add_option([{'Option',_Attrs,_Content}|_Rest] = Content) ->
    [{'Option', [{name,"UNIFIED_SRC_NODATA"}],["YES"]} | ["\n    "|Content]];
add_option([Head | Rest]) ->
    [Head | add_option(Rest)].

