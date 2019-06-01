-module(profile_test).

-include_lib("eunit/include/eunit.hrl").

-define(FEQUAL(E,F), ?assert(abs(E - F) < 0.00000001)).

tile_bounds_for_mercator_test() ->
    P = gdalnif2tiles:create_profile(mercator),
    {Tx, Ty, Tz} = {3302, 1749, 12},
    {EB0, EB1, EB2, EB3} = {12269060.28411021,-2925397.946530264,12278844.223730713,-2915614.0069097616},
    TB = gdalnif2tiles:tile_bounds(P, Tx, Ty, Tz),
    {B0, B1, B2, B3} = TB,
    ?debugFmt("tileBounds(~p, ~p, ~p) = ~p", [Tx, Ty, Tz, TB]),
    ?FEQUAL(EB0, B0),
    ?FEQUAL(EB1, B1),
    ?FEQUAL(EB2, B2),
    ?FEQUAL(EB3, B3).

tile_bounds_for_geodetic_test() ->
    P = gdalnif2tiles:create_profile(geodetic),
    {Tx, Ty, Tz} = {3302, 1749, 12},
    {EB0, EB1, EB2, EB3} = {110.21484375, 63.720703125, 110.302734375, 63.80859375},
    TB = gdalnif2tiles:tile_bounds(P, Tx, Ty, Tz),
    {B0, B1, B2, B3} = TB,
    ?debugFmt("tileBounds(~p, ~p, ~p) = ~p", [Tx, Ty, Tz, TB]),
    ?FEQUAL(EB0, B0),
    ?FEQUAL(EB1, B1),
    ?FEQUAL(EB2, B2),
    ?FEQUAL(EB3, B3).
    
tile_bounds_for_tms_geodetic_test() ->
    P = gdalnif2tiles:create_profile(geodetic, tmscompatible),
    {Tx, Ty, Tz} = {6604, 3498, 12},
    {EB0, EB1, EB2, EB3} = {110.21484375, 63.720703125, 110.2587890625, 63.7646484375},
    TB = gdalnif2tiles:tile_bounds(P, Tx, Ty, Tz),
    {B0, B1, B2, B3} = TB,
    ?debugFmt("tileBounds(~p, ~p, ~p) = ~p", [Tx, Ty, Tz, TB]),
    ?FEQUAL(EB0, B0),
    ?FEQUAL(EB1, B1),
    ?FEQUAL(EB2, B2),
    ?FEQUAL(EB3, B3).
    
output_bounds_mercator_test() ->
    Info = #{bandCount => 2,driverLongName => "Virtual Raster",
             driverShortName => "VRT",
             origin => {13692281.906532262,7558443.30498089},
             pixelSize => {44.36788316627073,-44.36788316627073},
             rasterHeight => 4431,rasterWidth => 2510},
    OB = global_profile:output_bounds(Info),
    #{ominx := OBminx, omaxx := OBmaxx, omaxy := OBmaxy, ominy := OBminy} = OB,
    ?debugFmt("Bounds (output srs): ~p", [OB]),
    {Eominx,Eominy,Eomaxx,Eomaxy} = {13692281.906532262, 7361849.214671144, 13803645.293279601, 7558443.30498089},
    ?FEQUAL(Eominx, OBminx),
    ?FEQUAL(Eominy, OBminy),
    ?FEQUAL(Eomaxx, OBmaxx),
    ?FEQUAL(Eomaxy, OBmaxy).

output_bounds_geodetic_test() ->
    Info = #{bandCount => 2,driverLongName => "Virtual Raster",
             driverShortName => "VRT",
             origin => {122.999861111111116,56.000138888888891},
             pixelSize => {0.000277777777778,-0.000277777777778},
             rasterHeight => 3601,
             rasterWidth => 3601},
    OB = global_profile:output_bounds(Info),
    #{ominx := OBminx, omaxx := OBmaxx, omaxy := OBmaxy, ominy := OBminy} = OB,
    ?debugFmt("Bounds (output srs): ~p", [OB]),
    {Eominx,Eominy,Eomaxx,Eomaxy} = {122.9998611111111, 54.999861111111116, 124.0001388888889, 56.00013888888889},
    ?FEQUAL(Eominx, OBminx),
    ?FEQUAL(Eominy, OBminy),
    ?FEQUAL(Eomaxx, OBmaxx),
    ?FEQUAL(Eomaxy, OBmaxy).

mercator_meters_to_tile_test() ->
    Profile = #{ profile => mercator, tileSize => 256 },
    {Tx,Ty} = global_profile:units_to_tile(Profile, 13692281.906532262, 7361849.214671144, 11),
    ?assertEqual(1723, Tx),
    ?assertEqual(1400, Ty).

geodetic_lonlat_to_tile_test() ->
    Profile = #{ profile => geodetic, tileSize => 256 },
    Lon = 122.999861, Lat = 54.999861,
    Zoom = 11,
    {Tx, Ty} = global_profile:units_to_tile(Profile, Lon, Lat, Zoom),
    ?assertEqual(1723, Tx),
    ?assertEqual(824, Ty).

