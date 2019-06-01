-module(global_profile).

-export([tile_minmax_zoom/2]).
-export([zoom_extents_for/2]).

-export([tile_bounds/4]).
-export([output_bounds/1]).
-export([units_to_tile/4]).

-ifdef(TEST).
-export([zoom4pixelsize/2]).
-endif.

-type profile()    :: map().
-type raster_info():: map().
-type zoom_range() :: 0..32.

-export_type([profile/0]).
-export_type([zoom_range/0]).

%% Output Bounds - coordinates in the output SRS
-spec output_bounds(raster_info()) -> map().
output_bounds(RasterInfo) ->
    #{origin := O, pixelSize := PS, rasterWidth := XSize, rasterHeight := YSize} = RasterInfo,
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

-spec tile_bounds(profile(), non_neg_integer(), non_neg_integer(), zoom_range()) -> {float(), float(), float(), float()}.
tile_bounds(#{tileSize := TileSize} = Profile, TX, TY, TZ) ->
    InitialResolution = initialResolution(Profile),
    Resolution = resolution(InitialResolution, TZ),
    {Xmin, Ymin} = pixels2units(TX * TileSize, TY * TileSize, Resolution, originShift(Profile)),
    {Xmax, Ymax} = pixels2units((TX + 1) * TileSize, (TY + 1) * TileSize, Resolution, originShift(Profile)),
    {Xmin, Ymin, Xmax, Ymax}.

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
    #{pixelSize := PS, rasterWidth := XSize, rasterHeight := YSize} = RasterInfo,
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

