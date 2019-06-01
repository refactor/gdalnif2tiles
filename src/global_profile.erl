-module(global_profile).

-export([output_bounds/1]).
-export([units_to_tile/4]).

-type profile()    :: map().
-type zoom_range() :: 0..32.

-export_type([profile/0]).
-export_type([zoom_range/0]).

-spec output_bounds(map()) -> profile().
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

%% Returns tile for given mercator/geodetic coordinates
%% unit: meters for Mercator; deg for Geodetic
-spec units_to_tile(map(), float(), float(), zoom_range()) -> {non_neg_integer(), non_neg_integer()}.
units_to_tile(#{tileSize := TileSize} = Profile, X, Y, TZ) ->
    %% Returns tile for given mercator coordinates
    %% mercator.MetersToTile(...)
    InitialResolution = initialResolution(Profile),
    Resolution = resolution(InitialResolution, TZ),
    {Px, Py} = units2pixels(X, Y, Resolution, originShift(Profile)),
    pixels2tile(Px, Py, TileSize).

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
-spec initialResolution(map()) -> float().
initialResolution(#{profile := mercator, tileSize := TileSize}) ->
    %% 156543.03392804062 for tile_size 256 pixels
    2 * math:pi() * 6378137 / TileSize;
initialResolution(#{profile := geodetic, tileSize := TileSize} = Profile) ->
    case maps:get(tmscompatible, Profile, none) of
        none ->
            360.0 / TileSize;
        _ ->
            180.0 / TileSize
    end.

-spec originShift(map()) -> {float(), float()}.
originShift(#{profile := mercator}) ->
    %% 20037508.342789244
    S = 2 * math:pi() * 6378137 / 2.0,
    {S, S};
originShift(#{profile := geodetic}) ->
    {180.0, 90.0}.

