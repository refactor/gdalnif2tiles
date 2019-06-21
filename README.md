
```bash
$ ./my-admin.sh gdal2tiles i=/bigdata/test_data/nairobi_tm5_1986005_geo.tif o=data -p mercator
```


```erl
gdal2tiles_manager:kickoff_tileworkers("/test_data/harvey_tmo_2017243_swir_geo.tif", world_profile:init(mercator)).
```
