-module(gdalnif2tiles).

-include_lib("kernel/include/logger.hrl").

-export([create_profile/1]).
-export([open_file/1]).
-export([has_nodata/1]).
-export([info/1]).
-export([band_info/2]).
-export([get_pixel/3]).
-export([tile_bounds/4]).

-export([assign_profile/2]).
-export([reproj2profile/2]).
-export([get_xmlvrt/1]).

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

%% @private
-spec has_nodata(reference()) -> string() | none.
has_nodata(_Dataset) ->
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

-spec assign_profile(reference(), reference()) -> reference().
assign_profile(Dataset, Profile) ->
    WDataset = reproj2profile(Dataset, Profile),
    Nodata = has_nodata(Dataset),
    logger:debug("Nodata: ~ts~n", [Nodata]),
    case Nodata of
        none ->
            pass;
        _ ->
            update_no_data_values(WDataset, Nodata)
    end,
    WDataset.

-spec reproj2profile(reference(), reference()) -> reference().
reproj2profile(_Dataset, _Profile) ->
    erlang:nif_error(notfound).

-spec get_xmlvrt(reference()) -> string().
get_xmlvrt(_WDataset) ->
    erlang:nif_error(notfound).

%% @private
-spec update_no_data_values(reference(), string()) -> reference().
update_no_data_values(WDataset, Nodata) ->
    Str = get_xmlvrt(WDataset),
    {XmlDoc,_} = xmerl_scan:string(Str),
    TL = xmerl_lib:simplify_element(XmlDoc),
    NewTL = add_gdal_warp_options_to_string(TL),
    % rm header: "<?xml version=\"1.0\"?>"
    CorrectedStr = binary:list_to_bin(tl(xmerl:export_simple([NewTL], xmerl_xml))),
    correct_dataset(WDataset, CorrectedStr, Nodata).

%% @private
-spec correct_dataset(reference(), binary(), binary()) -> reference().
correct_dataset(_Dataset, _CorrectedStr, _Nodata) ->
    erlang:nif_error(notfound).

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

-spec tile_bounds(reference(), non_neg_integer(), non_neg_integer(), 0..24) -> {float(),float(),float(),float()}.
tile_bounds(_Dataset, _tx, _ty, _tz) ->
    erlang:nif_error(notfound).
