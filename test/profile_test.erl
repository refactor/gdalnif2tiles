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
    
