-module(global_profile).

-include_lib("kernel/include/logger.hrl").

-export([init/1]).

-export([tile_bounds/4]).
-export([output_bounds/1]).
-export([generate_base_tiles/2]).
-export([base_tiles_bounds/2]).

-export([units_to_tile/4]).
-export([tilefilename/3]).
-ifdef(TEST).
-export([new_tile_job/2]).
-export([geo_query/2]).
-export([tile_minmax_zoom/2]).
-export([zoom_extents_for/2]).
-export([zoom4pixelsize/2]).
-endif.

-type tile_job_info() :: map().
-type tile_detail()   :: map().
-type tile_bounds()   :: {float(), float(), float(), float()}.
-type tiles_range()   :: {non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type pixel_window()  :: {non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type profile()       :: map().
-type raster_info()   :: map().
-type zoom_range()    :: 0..32.

-export_type([tile_job_info/0]).
-export_type([tile_bounds/0]).
-export_type([profile/0]).
-export_type([zoom_range/0]).

-spec init(mercator | geodetic | {geodetic,tmscomatible} | map()) -> profile().
init(mercator) ->
    P  = #{ profile => mercator, tileSize => 256 },
    init(P);
init(geodetic) ->
    P  = #{ profile => geodetic, tileSize => 256 },
    init(P);
init({geodetic, tmscompatible}) ->
    P  = #{ profile => geodetic, tileSize => 256, tmscompatible => true },
    init(P);
init(#{tileSize := TileSize} = P) ->
%    P1 = maps:put(querysize, TileSize, P),
    P2 = maps:put(tiledriver, 'PNG', P),
    P2.

%% Generation of the base tiles (the lowest in the pyramid) directly from the input raster
-spec generate_base_tiles(profile(), raster_info()) -> {tile_job_info(), [tile_detail()]}.
generate_base_tiles(Profile, RasterInfo) ->
    {RasterProfile, ZoomExtents} = new_tile_job(Profile, RasterInfo),
    
    % Set the bounds
    #{tzmax := Zmax} = RasterProfile,
    {Tminx, Tminy, Tmaxx, Tmaxy} = base_tiles_bounds(Zmax, ZoomExtents),
    TL = [{X, Y} || X <- lists:seq(Tminx, Tmaxx), Y <- lists:seq(Tmaxy, Tminy, -1)],
    TileDetails = lists:map(fun({Tx, Ty}) -> generate_base_tile(RasterProfile, Tx, Ty, Zmax) end, TL),

    {RasterProfile, TileDetails}.

%% @private
-spec new_tile_job(profile(), raster_info()) -> {tile_job_info(), [tiles_range()]}.
new_tile_job(Profile, RasterInfo) ->
    {Zmin,Zmax} = tile_minmax_zoom(Profile, RasterInfo),
    ZoomExtents = zoom_extents_for(Profile, RasterInfo),
    RasterProfile = maps:merge(RasterInfo, Profile),
    {RasterProfile#{tzmin => Zmin, tzmax => Zmax}, ZoomExtents}.

%% @private
-spec generate_base_tile(tile_job_info(), non_neg_integer(), non_neg_integer(), zoom_range()) -> tile_detail().
generate_base_tile(RasterProfile, Tx, Ty, TZ) ->
    B = tile_bounds(RasterProfile, Tx, Ty, TZ),
    {RB, WB} = geo_query(RasterProfile, B),
    {RX, RY, RXSize, RYSize} = RB,
    {WX, WY, WXSize, WYSize} = WB,
    #{tx => Tx, ty => Ty, tz => TZ, rx => RX, ry => RY, rxsize => RXSize, rysize => RYSize,
      wx => WX, wy => WY, wxsize => WXSize, wysize => WYSize,
      querysize => maps:get(querysize, RasterProfile, undefined)}.

tilefilename(Tx, Ty, TZ) ->
    TileFilenameKey = string:trim(io_lib:format("~p/~p/~p.png", [Tx, Ty, TZ])),
    TileFilenameKey.

%% For given dataset and query in cartographic coordinates returns parameters for ReadRaster()
%% in raster coordinates and x/y shifts (for border tiles).
%% If the querysize is not given, the extent is returned in the native resolution of dataset ds.
%%
%% raises Gdal2TilesError if the dataset does not contain anything inside this geo_query
-spec geo_query(raster_info(), tile_bounds()) -> {pixel_window(), pixel_window()}.
geo_query(RasterInfo, TileBounds) ->
    #{origin := O, pixelSize := PS, rasterSize := {RasterXSize, RasterYSize} } = RasterInfo,
    {OX, OY} = O,
    {PSX,PSY} = PS,
    %% ulx, uly, lrx, lry
    {ULx, LRy, LRx, ULy} = TileBounds,
    RX = trunc((ULx - OX) / PSX + 0.001),
    RY = trunc((ULy - OY) / PSY + 0.001),
    RXSize = trunc((LRx - ULx) / PSX + 0.5),
    RYSize = trunc((LRy - ULy) / PSY + 0.5),
    {WXSize,WYSize} = 
        case maps:get(querysize, RasterInfo, undefined) of
            undefined ->
                {RXSize, RYSize};
            QuerySize ->
                {QuerySize, QuerySize}
        end,
    {NewRX, NewRXSize, WX, NewWXSize} = adjust_coordinate(RX, RXSize, WXSize, RasterXSize), 
    {NewRY, NewRYSize, WY, NewWYSize} = adjust_coordinate(RY, RYSize, WYSize, RasterYSize), 
    {{NewRX, NewRY, NewRXSize, NewRYSize}, {WX, WY, NewWXSize, NewWYSize}}.

%% Coordinates should not go out of the bounds of the raster
adjust_coordinate(RCoord, RSize, WSize, RasterSize) ->
    {NewRCoord, NewRSize0, W, NewWSize0} = 
        case (RCoord < 0) of
            true ->
               RShift = abs(RCoord),
               AW = trunc(WSize * (float(RShift) / RSize)),
               AWSize = WSize - AW,
               ARSize = RSize - trunc(RSize * (float(RShift) / RSize)),
               ARCoord = 0,
               {ARCoord, ARSize, AW, AWSize};
           false ->
               {RCoord, RSize, 0, WSize}
        end,
    {NewRSize, NewWSize} = 
        case ((NewRCoord + NewRSize0) > RasterSize) of
            true ->
               AWSize1 = trunc(NewWSize0 * (float(RasterSize - NewRCoord) / NewRSize0)),
               ARSize1 = RasterSize - NewRCoord,
               {ARSize1, AWSize1};
           false ->
               {NewRSize0, NewWSize0}
        end,
    {NewRCoord, NewRSize, W, NewWSize}.

-spec base_tiles_bounds(zoom_range(), [tiles_range()]) -> tiles_range().
base_tiles_bounds(Zmax, ZoomExtents) ->
    lists:nth(Zmax + 1, ZoomExtents).

-spec tile_bounds(profile(), non_neg_integer(), non_neg_integer(), zoom_range()) -> tile_bounds().
tile_bounds(#{tileSize := TileSize} = Profile, TX, TY, TZ) ->
    InitialResolution = initialResolution(Profile),
    Resolution = resolution(InitialResolution, TZ),
    {Xmin, Ymin} = pixels2units(TX * TileSize, TY * TileSize, Resolution, originShift(Profile)),
    {Xmax, Ymax} = pixels2units((TX + 1) * TileSize, (TY + 1) * TileSize, Resolution, originShift(Profile)),
    {Xmin, Ymin, Xmax, Ymax}.

%% Output Bounds - coordinates in the output SRS
-spec output_bounds(raster_info()) -> map().
output_bounds(RasterInfo) ->
    #{origin := O, pixelSize := PS, rasterSize := {XSize, YSize}} = RasterInfo,
    {Ominx, Omaxy} = O,
    {PixelXsize, PixelYsize} = PS,
    OBminx = Ominx,
    OBmaxx = Ominx + XSize * PixelXsize,
    OBmaxy = Omaxy,

    %% Is this a gdal2tiles.py bug?
    %%  self.ominy = self.out_gt[3] - self.warped_input_dataset.RasterYSize * self.out_gt[1]
    %% instead, use this one:
    %%  self.ominy = self.out_gt[3] + self.warped_input_dataset.RasterYSize * self.out_gt[5]
    OBminy = Omaxy + YSize * PixelYsize,
    #{ominx => OBminx, omaxx => OBmaxx, omaxy => OBmaxy, ominy => OBminy}.

%% Returns tile for given mercator/geodetic coordinates
%% unit: meters for Mercator; deg for Geodetic
-spec units_to_tile(profile(), float(), float(), zoom_range()) -> {non_neg_integer(), non_neg_integer()}.
units_to_tile(#{tileSize := TileSize} = Profile, X, Y, TZ) ->
    %% Returns tile for given mercator coordinates
    %% mercator.MetersToTile(...)
    InitialResolution = initialResolution(Profile),
    Resolution = resolution(InitialResolution, TZ),
    {Px, Py} = units2pixels(X, Y, Resolution, originShift(Profile)),
    pixels2tile(Px, Py, TileSize).

%% Get the minimal zoom level (map covers area equivalent to one tile) and
%% Get the maximal zoom level (closest possible zoom level up on the resolution of raster)
-spec tile_minmax_zoom(profile(), raster_info()) -> {zoom_range(), zoom_range()}.
tile_minmax_zoom(#{tileSize := TileSize} = Profile, RasterInfo) ->
    #{pixelSize := PS, rasterSize := {XSize, YSize}} = RasterInfo,
    {PixelXsize, _PixelYsize} = PS,
    Tmaxz = zoom4pixelsize(Profile, PixelXsize),
    ZPixelSize = PixelXsize * max(XSize,YSize) / TileSize,
    Tminz = zoom4pixelsize(Profile, ZPixelSize),
    {Tminz, Tmaxz}.

%% Generate table with min max tile coordinates for all zoomlevels
-spec zoom_extents_for(profile(), raster_info()) -> [{non_neg_integer(),non_neg_integer(),non_neg_integer(),non_neg_integer()}].
zoom_extents_for(Profile, RasterInfo) ->
    OB = output_bounds(RasterInfo),
    zoom_extents_for(Profile, OB, [], 31).

%% @private
zoom_extents_for(_Profile, _OB, L, 0) ->
    ZE = {0,0,0,0},
    [ZE | L];
zoom_extents_for(Profile, OB, L, TZ) ->
    #{ominx := OBminx, omaxx := OBmaxx, omaxy := OBmaxy, ominy := OBminy} = OB,
    {Tminx0, Tminy0} = units_to_tile(Profile, OBminx, OBminy, TZ),
    {Tmaxx0, Tmaxy0} = units_to_tile(Profile, OBmaxx, OBmaxy, TZ),
    Tminx = max(0, Tminx0),
    Tminy = max(0, Tminy0),
    Tmaxx = min((1 bsl (TZ + 1)) - 1, Tmaxx0),
    Tmaxy = min((1 bsl TZ) - 1, Tmaxy0),
    zoom_extents_for(Profile, OB, [{Tminx, Tminy, Tmaxx, Tmaxy} | L], TZ - 1).

%% Maximal scaledown zoom of the pyramid closest to the pixelSize
-spec zoom4pixelsize(profile(), float()) -> zoom_range().
zoom4pixelsize(Profile, PixelSize) ->
    InitialResolution = initialResolution(Profile),
    zoom4pixelsize(InitialResolution, PixelSize, 0).

%% @private
-spec zoom4pixelsize(float(), float(), zoom_range()) -> zoom_range().
zoom4pixelsize(_InitialResolution, _PixelSize, 31) ->
    31 -1;
zoom4pixelsize(InitialResolution, PixelSize, Z) ->
    Resolution = resolution(InitialResolution, Z),
    if (abs(PixelSize) > Resolution) ->
        max(0, Z - 1);
    true ->
        zoom4pixelsize(InitialResolution, PixelSize, Z + 1)
    end.

pixels2units(PX, PY, Resolution, {OriginShiftX, OriginShiftY}) ->
    X = PX * Resolution - OriginShiftX,
    Y = PY * Resolution - OriginShiftY,
    {X, Y}.

%% @private
%% Converts EPSG:3857 to pyramid pixel coordinates in given zoom level
-spec units2pixels(float(), float(), float(), {float(),float()}) -> {float(),float()}.
units2pixels(MX, MY, Resolution, {OriginShiftX, OriginShiftY}) ->
    PX = (MX + OriginShiftX) / Resolution,
    PY = (MY + OriginShiftY) / Resolution,
    {PX, PY}.

%% @private
%% Returns coordinates of the tile covering region in pixel coordinates
-spec pixels2tile(float(), float(), non_neg_integer()) -> {non_neg_integer(), non_neg_integer()}.
pixels2tile(PX, PY, TileSize) ->
    Tx = ceil(PX / TileSize) - 1,
    Ty = ceil(PY / TileSize) - 1,
    {Tx, Ty}.

%% @private
%% Resolution (meters/pixel) for given zoom level (measured at Equator)
-spec resolution(float(), zoom_range()) -> float().
resolution(InitialResolution, Zoom) ->
    InitialResolution / math:pow(2, Zoom).


%% @private
-spec initialResolution(profile()) -> float().
initialResolution(#{profile := mercator, tileSize := TileSize}) ->
    %% 156543.03392804062 for tile_size 256 pixels
    2 * math:pi() * 6378137 / TileSize;
initialResolution(#{profile := geodetic, tileSize := TileSize} = Profile) ->
    case maps:get(tmscompatible, Profile, none) of
        none ->
            360.0 / TileSize;
        false ->
            360.0 / TileSize;
        _ ->
            180.0 / TileSize
    end.

-spec originShift(profile()) -> {float(), float()}.
originShift(#{profile := mercator}) ->
    %% 20037508.342789244
    S = 2 * math:pi() * 6378137 / 2.0,
    {S, S};
originShift(#{profile := geodetic}) ->
    {180.0, 90.0}.

