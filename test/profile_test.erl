-module(profile_test).

-include_lib("eunit/include/eunit.hrl").

-define(FEQUAL(E,F), ?assert(abs(E - F) < 0.00000001)).

tile_bounds_for_mercator_test() ->
    Profile = world_profile:init(mercator),
    {Tx, Ty, Tz} = {3302, 1749, 12},
    {EB0, EB1, EB2, EB3} = {12269060.28411021,-2925397.946530264,12278844.223730713,-2915614.0069097616},
    TB = world_profile:tile_bounds(Profile, Tx, Ty, Tz),
    {B0, B1, B2, B3} = TB,
    ?debugFmt("tileBounds(~p, ~p, ~p) = ~p", [Tx, Ty, Tz, TB]),
    ?FEQUAL(EB0, B0),
    ?FEQUAL(EB1, B1),
    ?FEQUAL(EB2, B2),
    ?FEQUAL(EB3, B3).

tile_bounds_for_geodetic_test() ->
    Profile = world_profile:init(geodetic),
    {Tx, Ty, Tz} = {3302, 1749, 12},
    {EB0, EB1, EB2, EB3} = {110.21484375, 63.720703125, 110.302734375, 63.80859375},
    TB = world_profile:tile_bounds(Profile, Tx, Ty, Tz),
    {B0, B1, B2, B3} = TB,
    ?debugFmt("tileBounds(~p, ~p, ~p) = ~p", [Tx, Ty, Tz, TB]),
    ?FEQUAL(EB0, B0),
    ?FEQUAL(EB1, B1),
    ?FEQUAL(EB2, B2),
    ?FEQUAL(EB3, B3).
    
tile_bounds_for_tms_geodetic_test() ->
    Profile = world_profile:init({geodetic, tmscompatible}),
    {Tx, Ty, Tz} = {6604, 3498, 12},
    {EB0, EB1, EB2, EB3} = {110.21484375, 63.720703125, 110.2587890625, 63.7646484375},
    TB = world_profile:tile_bounds(Profile, Tx, Ty, Tz),
    {B0, B1, B2, B3} = TB,
    ?debugFmt("tileBounds(~p, ~p, ~p) = ~p", [Tx, Ty, Tz, TB]),
    ?FEQUAL(EB0, B0),
    ?FEQUAL(EB1, B1),
    ?FEQUAL(EB2, B2),
    ?FEQUAL(EB3, B3).
    
output_bounds_mercator_test() ->
    RasterInfo = #{bandCount => 2,driverLongName => "Virtual Raster",
             driverShortName => "VRT",
             origin => {13692281.906532262,7558443.30498089},
             pixelSize => {44.36788316627073,-44.36788316627073},
             rasterSize => {2510, 4431}},
    OB = world_profile:output_bounds(RasterInfo),
    #{ominx := OBminx, omaxx := OBmaxx, omaxy := OBmaxy, ominy := OBminy} = OB,
    ?debugFmt("Bounds (output srs): ~p", [OB]),
    {Eominx,Eominy,Eomaxx,Eomaxy} = {13692281.906532262, 7361849.214671144, 13803645.293279601, 7558443.30498089},
    ?FEQUAL(Eominx, OBminx),
    ?FEQUAL(Eominy, OBminy),
    ?FEQUAL(Eomaxx, OBmaxx),
    ?FEQUAL(Eomaxy, OBmaxy).

output_bounds_geodetic_test() ->
    RasterInfo = #{bandCount => 2,driverLongName => "Virtual Raster",
             driverShortName => "VRT",
             origin => {122.999861111111116,56.000138888888891},
             pixelSize => {0.000277777777778,-0.000277777777778},
             rasterSize => {3601, 3601}},
    OB = world_profile:output_bounds(RasterInfo),
    #{ominx := OBminx, omaxx := OBmaxx, omaxy := OBmaxy, ominy := OBminy} = OB,
    ?debugFmt("Bounds (output srs): ~p", [OB]),
    {Eominx,Eominy,Eomaxx,Eomaxy} = {122.9998611111111, 54.999861111111116, 124.0001388888889, 56.00013888888889},
    ?FEQUAL(Eominx, OBminx),
    ?FEQUAL(Eominy, OBminy),
    ?FEQUAL(Eomaxx, OBmaxx),
    ?FEQUAL(Eomaxy, OBmaxy).

mercator_meters_to_tile_test() ->
    Profile = world_profile:init(mercator),
    {Tx,Ty} = world_profile:units_to_tile(Profile, 13692281.906532262, 7361849.214671144, 11),
    ?assertEqual(1723, Tx),
    ?assertEqual(1400, Ty).

geodetic_lonlat_to_tile_test() ->
    Profile = #{ profile => geodetic, tileSize => 256 },
    Lon = 122.999861, Lat = 54.999861,
    Zoom = 11,
    {Tx, Ty} = world_profile:units_to_tile(Profile, Lon, Lat, Zoom),
    ?assertEqual(1723, Tx),
    ?assertEqual(824, Ty).

zoom4pixelsize_geodetic_test() ->
    PixelSize = 0.000278,
    Profile = world_profile:init(geodetic),
    Z = world_profile:zoom4pixelsize(Profile, PixelSize),
    ?assertEqual(12, Z).
    
zoom4pixelsize_mercator_test() ->
    PixelSize = 44.367883,
    Profile = world_profile:init(mercator),
    Z = world_profile:zoom4pixelsize(Profile, PixelSize),
    ?assertEqual(11, Z).

zoom_for_pixelsize_geodetic_test() ->
    RasterInfo = #{bandCount => 2,driverLongName => "Virtual Raster",
             driverShortName => "VRT",
             origin => {122.999861111111116,56.000138888888891},
             pixelSize => {0.000277777777778,-0.000277777777778},
             rasterSize => {3601, 3601}},
    Profile = world_profile:init(geodetic),
    {Zmin,Zmax} = world_profile:tile_minmax_zoom(Profile, RasterInfo),
    ?assertEqual(8, Zmin),
    ?assertEqual(12, Zmax).

zoom_for_pixelsize_mercator_test() ->
    RasterInfo = #{bandCount => 2,driverLongName => "Virtual Raster",
             driverShortName => "VRT",
             origin => {13692281.906532262,7558443.30498089},
             pixelSize => {44.36788316627073,-44.36788316627073},
             rasterSize => {2510, 4431}},
    Profile = world_profile:init(mercator),
    {Zmin,Zmax} = world_profile:tile_minmax_zoom(Profile, RasterInfo),
    ?assertEqual(7, Zmin),
    ?assertEqual(11, Zmax).

zoom_extents_geodetic_test() ->
    RasterInfo = #{bandCount => 2,driverLongName => "Virtual Raster",
             driverShortName => "VRT",
             origin => {122.999861111111116,56.000138888888891},
             pixelSize => {0.000277777777778,-0.000277777777778},
             rasterSize => {3601, 3601}},
    Profile = world_profile:init(geodetic),
    ZoomExtents = world_profile:zoom_extents_for(Profile, RasterInfo),
    ExpectedZoomExtents = [
        {0, 0, 0, 0},
        {1, 0, 1, 0},
        {3, 1, 3, 1},
        {6, 3, 6, 3},
        {13, 6, 13, 6},
        {26, 12, 27, 12},
        {53, 25, 54, 25},
        {107, 51, 108, 51},
        {215, 103, 216, 103},
        {430, 206, 432, 207},
        {861, 412, 864, 415},
        {1723, 824, 1729, 830},
        {3447, 1649, 3458, 1661},
        {6894, 3299, 6917, 3322},
        {13789, 6599, 13835, 6644},
        {27579, 13198, 27670, 13289},
        {55159, 26396, 55341, 26578},
        {110318, 52792, 110683, 53157},
        {220637, 105585, 221366, 106314},
        {441275, 211171, 442732, 212628},
        {882551, 422342, 885464, 425256},
        {1765102, 844685, 1770929, 850512},
        {3530204, 1689370, 3541858, 1701024},
        {7060408, 3378741, 7083716, 3402049},
        {14120816, 6757483, 14167433, 6804099},
        {28241633, 13514966, 28334866, 13608199},
        {56483267, 27029933, 56669733, 27216398},
        {112966535, 54059866, 113339466, 54432797},
        {225933071, 108119732, 226678933, 108865594},
        {451866143, 216239465, 453357866, 217731188},
        {903732287, 432478931, 906715732, 435462376},
        {1807464575, 864957863, 1813431464, 870924752}
    ],
    ?assertEqual(ExpectedZoomExtents, ZoomExtents).

zoom_extents_mercator_test() ->
    Info = #{bandCount => 2,driverLongName => "Virtual Raster",
             driverShortName => "VRT",
             origin => {13692281.906532262,7558443.30498089},
             pixelSize => {44.36788316627073,-44.36788316627073},
             rasterSize => {2510, 4431}},
    Profile = world_profile:init(mercator),
    ZoomExtents = world_profile:zoom_extents_for(Profile, Info),
    ExpectedZoomExtents = [
        {0, 0, 0, 0},
        {1, 1, 1, 1},
        {3, 2, 3, 2},
        {6, 5, 6, 5},
        {13, 10, 13, 11},
        {26, 21, 27, 22},
        {53, 43, 54, 44},
        {107, 87, 108, 88},
        {215, 175, 216, 176},
        {430, 350, 432, 352},
        {861, 700, 864, 705},
        {1723, 1400, 1729, 1410},
        {3447, 2800, 3458, 2820},
        {6894, 5600, 6917, 5641},
        {13789, 11201, 13835, 11282},
        {27579, 22403, 27670, 22564},
        {55159, 44807, 55341, 45128},
        {110318, 89614, 110683, 90257},
        {220637, 179228, 221366, 180514},
        {441275, 358456, 442732, 361028},
        {882551, 716913, 885464, 722057},
        {1765102, 1433826, 1770929, 1444114},
        {3530204, 2867652, 3541859, 2888228},
        {7060408, 5735305, 7083719, 5776457},
        {14120816, 11470611, 14167438, 11552914},
        {28241633, 22941222, 28334877, 23105829},
        {56483267, 45882445, 56669754, 46211658},
        {112966535, 91764890, 113339509, 92423316},
        {225933071, 183529781, 226679019, 184846632},
        {451866143, 367059562, 453358039, 369693264},
        {903732287, 734119124, 906716079, 739386528},
        {1807464575, 1468238248, 1813432159, 1478773056}
    ],
    ?assertEqual(ExpectedZoomExtents, ZoomExtents).

base_tiles_bounds_mercator_test() ->
    RasterInfo = #{bandCount => 2,driverLongName => "Virtual Raster",
             driverShortName => "VRT",
             origin => {13692281.906532262,7558443.30498089},
             pixelSize => {44.36788316627073,-44.36788316627073},
             rasterSize => {2510, 4431}},
    Profile0 = world_profile:init(mercator),
    {RasterProfile,ZoomExtents} = world_profile:new_tile_job(Profile0, RasterInfo),
    #{tzmax := TZ} = RasterProfile,
    ZoomExtent = world_profile:base_tiles_bounds(TZ, ZoomExtents),
    ExpectedZE = {1723, 1400, 1729, 1410},
    ?assertEqual(11, TZ),
    ?assertEqual(ExpectedZE, ZoomExtent).

base_tiles_bounds_geodetic_test() ->
    RasterInfo = #{bandCount => 2,driverLongName => "Virtual Raster",
             driverShortName => "VRT",
             origin => {122.999861111111116,56.000138888888891},
             pixelSize => {0.000277777777778,-0.000277777777778},
             rasterSize => {3601, 3601}},
    Profile0 = world_profile:init(geodetic),
    {RasterProfile,ZoomExtents} = world_profile:new_tile_job(Profile0, RasterInfo),
    #{tzmax := TZ} = RasterProfile,
    ZoomExtent = world_profile:base_tiles_bounds(TZ, ZoomExtents),
    ExpectedZE = {3447, 1649, 3458, 1661},
    ?assertEqual(12, TZ),
    ?assertEqual(ExpectedZE, ZoomExtent).

geo_query_geodetic_test() ->
    RasterInfo = #{bandCount => 2,driverLongName => "Virtual Raster",
             driverShortName => "VRT",
             origin => {122.999861111111116,56.000138888888891},
             pixelSize => {0.000277777777778,-0.000277777777778},
             rasterSize => {3601,  3601}},
    Profile0 = world_profile:init(geodetic),
    Profile = maps:remove(querysize, Profile0),
    {RasterProfile,_} = world_profile:new_tile_job(Profile, RasterInfo),
    TileBounds = {122.958984375, 55.986328125, 123.046875, 56.07421875},
    {RB,WB} = world_profile:geo_query(RasterProfile, TileBounds),
    ExpectedRB = {0, 0, 169, 50},
    ExpectedWB = {147, 266, 169, 50},
    ?assertEqual(ExpectedRB, RB),
    ?assertEqual(ExpectedWB, WB).

geo_query_mercator_test() ->
    RasterInfo = #{bandCount => 2,driverLongName => "Virtual Raster",
             driverShortName => "VRT",
             origin => {13692281.906532262,7558443.30498089},
             pixelSize => {44.36788316627073,-44.36788316627073},
             rasterSize => {2510, 4431}},
    Profile0 = world_profile:init(mercator),
    Profile = Profile0#{querysize => 1024},
    {RasterProfile,_} = world_profile:new_tile_job(Profile, RasterInfo),
    TileBounds = {13677947.589462582, 7553201.387027975, 13697515.468703587, 7572769.2662689835},
    {RB,WB} = world_profile:geo_query(RasterProfile, TileBounds),
    ExpectedRB = {0, 0, 118, 119},
    ExpectedWB = {750, 747, 274, 277},
    ?assertEqual(ExpectedRB, RB),
    ?assertEqual(ExpectedWB, WB).

geo_query_mercator_with_querysize1_test() ->
    RasterInfo = #{bandCount => 2,driverLongName => "Virtual Raster",
             driverShortName => "VRT",
             origin => {13692281.906532262,7558443.30498089},
             pixelSize => {44.36788316627073,-44.36788316627073},
             rasterSize => {2510, 4431}},
    Profile0 = world_profile:init(mercator),
    Profile = Profile0#{querysize => 441},
    {RasterProfile,_} = world_profile:new_tile_job(Profile, RasterInfo),
    TileBounds = {13677947.589462582, 7553201.387027975, 13697515.468703587, 7572769.2662689835},
    {RB,WB} = world_profile:geo_query(RasterProfile, TileBounds),
    ExpectedRB = {0, 0, 118, 119},
    ExpectedWB = {323, 322, 118, 119},
    ?assertEqual(ExpectedRB, RB),
    ?assertEqual(ExpectedWB, WB).

geo_query_mercator_with_querysize2_test() ->
    RasterInfo = #{bandCount => 2,driverLongName => "Virtual Raster",
             driverShortName => "VRT",
             origin => {13692281.906532262,7558443.30498089},
             pixelSize => {44.36788316627073,-44.36788316627073},
             rasterSize => {2510, 4431}},
    Profile0 = world_profile:init(mercator),
    Profile = Profile0#{querysize => 1024},
    {RasterProfile,_} = world_profile:new_tile_job(Profile, RasterInfo),
    TileBounds = {13677947.589462582, 7553201.387027975, 13697515.468703587, 7572769.2662689835},
    {RB,WB} = world_profile:geo_query(RasterProfile, TileBounds),
    ExpectedRB = {0, 0, 118, 119},
    ExpectedWB = {750, 747, 274, 277},
    ?assertEqual(ExpectedRB, RB),
    ?assertEqual(ExpectedWB, WB).

geo_query_mercator_with_querysize_test() ->
    RasterInfo = #{bandCount => 2,driverLongName => "Virtual Raster",
             driverShortName => "VRT",
             origin => {-11050113.514904452487826,3979365.476011248305440},
             pixelSize => {287.051566576245023,-287.051566576245023},
             querysize => 1024,
             rasterSize => {4233, 4039}},
    Profile0 = world_profile:init(mercator),
    Profile = maps:remove(querysize, Profile0),
    {RasterProfile,_} = world_profile:new_tile_job(Profile, RasterInfo),
    TileBounds = {-10958012.374962868, 3913575.8482010253, -10879740.857998848, 3991847.3651650436},
    {RB,WB} = world_profile:geo_query(RasterProfile, TileBounds),
    ExpectedRB = {320, 0, 273, 230},
    ExpectedWB = {0, 161, 1024, 863},
    ?assertEqual(ExpectedRB, RB),
    ?assertEqual(ExpectedWB, WB).
