# MapofComputationalArchaeology

- The raster base map `Archaeologia.map` was created with this [Fantasy-Map-Generator](https://azgaar.github.io/Fantasy-Map-Generator) and can be edited with it as well. Do not activate additional displayed layers before saving.
- `./leaflet_prototype` contains the current, early stage of the version to be prepared for sharing on our website. The original template for this setup including some javascript code was copied from [here](https://github.com/commenthol/gdal2tiles-leaflet/tree/gh-pages). The version there also features some more inspiration how to expand the map.
- The process for creating a leaflet-compatible raster base map from the fantasy map is as follows:
	- Export and download tiles from the Fantasy-Map-Generator, e.g. in the lowest setting with 2\*2 tiles.
	- Merge individual tiles to one simple .png image with imagemagick (e.g. `montage -border 0 -geometry 800x -tile 2x2 fmg_tile_*.png final.jpg`, increase resolution for final run!).
	- Run [gdal2tiles.py](https://github.com/commenthol/gdal2tiles-leaflet/tree/master) on this image to create a version of the tiles compatible with the setup in `./leaflet_prototype` (e.g. `./gdal2tiles.py -l -p raster -w none final.jpg finaltiles`, see `gdal2tiles.py -h` for more info).
	- Copy the created tiles (`./finaltiles`) to `./tiles` in `./leaflet_prototype`