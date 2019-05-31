-module(gdalnif2tiles).

-include_lib("kernel/include/logger.hrl").

-export([create_profile/1]).
-export([create_profile/2]).
-export([open_file/1]).
-export([info/1]).
-export([band_info/2]).
-export([get_pixel/3]).
-export([warp_with_profile/2]).
-export([tile_bounds/4]).

-export([nb_data_bands/1]).
-export([reproj_with_profile/2]).
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

-spec create_profile('geodetic', 'tmscompatible') -> reference().
create_profile(_Profile, _Tmscompatible) ->
    erlang:nif_error(notfound).

-spec create_profile('mercator'|'geodetic') -> reference().
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

-spec warp_with_profile(reference(), reference()) -> reference().
warp_with_profile(Dataset, Profile) ->
    WDataset = reproj_with_profile(Dataset, Profile),
    try has_nodata(Dataset) of
        Nodata ->
            ?LOG_DEBUG("Nodata: ~ts", [Nodata]),
            update_no_data_values(WDataset, Nodata)
    catch
        error:Reason ->
            ?LOG_WARNING("try update_alpha_value_for_non_alpha_inputs for: ~p", [Reason]),
            update_alpha_value_for_non_alpha_inputs(WDataset)
    end.

-spec tile_bounds(reference(), non_neg_integer(), non_neg_integer(), 0..24) -> {float(),float(),float(),float()}.
tile_bounds(_Dataset, _tx, _ty, _tz) ->
    erlang:nif_error(notfound).

%% @private
-spec reproj_with_profile(reference(), reference()) -> reference().
reproj_with_profile(_Dataset, _Profile) ->
    erlang:nif_error(notfound).

%% @private
-spec has_nodata(reference()) -> string() | none.
has_nodata(_Dataset) ->
    erlang:nif_error(notfound).

%% @private
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
add_gdal_warp_options_to_string({Tag, Attributes, Content}) ->
    NewContent = add_gdal_warp_options(Content),
    {Tag, Attributes, NewContent}.

%% @private
add_gdal_warp_options([{'GDALWarpOptions',Attrs,Content} | Rest]) ->
    [{'GDALWarpOptions',Attrs, add_option(Content)} | Rest];
add_gdal_warp_options([Head|Rest]) ->
    [Head | add_gdal_warp_options(Rest)].

%% @private
add_option([{'Option',_Attrs,_Content}|_Rest] = Content) ->
    [{'Option', [{name,"UNIFIED_SRC_NODATA"}],["YES"]} | ["\n    "|Content]];
add_option([Head | Rest]) ->
    [Head | add_option(Rest)].

%% @private
update_alpha_value_for_non_alpha_inputs(WDataset) ->
    Band = raster_count(WDataset),
    if Band == 1 ; Band == 3 ->
        %% rasterCount in [1,3]    
        Str = get_xmlvrt(WDataset),
        {XmlDoc,_} = xmerl_scan:string(Str),
        TL = xmerl_lib:simplify_element(XmlDoc),
        NewTL = add_VRTRasterBand_to_string(Band, TL),
  %      ?LOG_DEBUG("~p", [NewTL]),
        CorrectedStr = binary:list_to_bin(tl(xmerl:export_simple([NewTL], xmerl_xml))),
        file:write_file("vtiles.vrt", CorrectedStr),
        correct_dataset(WDataset, CorrectedStr),
        WDataset;
    true ->
        WDataset
    end.

add_VRTRasterBand_to_string(Band, {Tag, Attributes, Content}) ->
    NewContent = add_VRTRasterBand_alpha(Band, Content),
    {Tag, Attributes, NewContent}.
    
add_VRTRasterBand_alpha(_Band, []) ->
    [];
add_VRTRasterBand_alpha(Band, [{'GDALWarpOptions',Attrs,Content} | Rest]) ->
    [{'GDALWarpOptions',Attrs, add_dstalpha_option(Band, Content)} | add_VRTRasterBand_alpha(Band, Rest)];
add_VRTRasterBand_alpha(Band, [{'VRTRasterBand',Attrs,_Content}=BandElem | Rest]) ->
    BC = list_to_integer(proplists:get_value('band', Attrs, "0")),
    if Band == BC ->
        AlphaBand = build_AlphaBand(Band),
        [BandElem | ["\n  " |[AlphaBand | add_VRTRasterBand_alpha(Band,Rest)]]];
    true ->
        [BandElem | add_VRTRasterBand_alpha(Band, Rest)]
    end;
add_VRTRasterBand_alpha(Band, [Head | Rest]) ->
    [Head | add_VRTRasterBand_alpha(Band, Rest)].

add_dstalpha_option(Band,  Content) ->
    BandStr = integer_to_list(Band + 1),
    Content ++ [{'DstAlphaBand', [], [BandStr]}, "\n  "].

build_AlphaBand(Band) ->
    BandStr = integer_to_list(Band + 1),
    {'VRTRasterBand', [{dataType,"Byte"},{'band',BandStr},{subClass,"VRTWarpedRasterBand"}], ["\n    ",{'ColorInterp', [], ["Alpha"]},"\n  "]}.

%% @private
-spec correct_dataset(reference(), binary(), binary()) -> reference().
correct_dataset(_Dataset, _VrtStr, _Nodata) ->
    erlang:nif_error(notfound).

%% @private
-spec correct_dataset(reference(), binary()) -> reference().
correct_dataset(_Dataset, _VrtStr) ->
    erlang:nif_error(notfound).

%% @private
raster_count(WDataset) ->
    #{bandCount := RC} = info(WDataset),
    RC.

%% Return the number of data (non-alpha) bands of a gdal dataset
nb_data_bands(_Dataset) ->
    erlang:nif_error(notfound).

