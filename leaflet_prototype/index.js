/*
 * @copyright 2015 commenthol
 * @license MIT
 */

/* globals L */

function init() {
	var minZoom = 0,
		maxZoom = 3,
		img = [
			2400,  // original width of image
			1174   // original height of image
		];

	// create the map
	var map = L.map('map',{
			minZoom: minZoom,
			maxZoom: maxZoom,
		});

	// assign map and image dimensions
	var rc = new L.RasterCoords(map, img);
	// set the bounds on map
	rc.setMaxBounds();

	// set the view on a marker ...
	map.setView(rc.unproject([1000, 1000]), 1);

	// set marker at the image bound edges
	var layerBounds = L.layerGroup([]);
	map.addLayer(layerBounds);

	// the tile layer containing the image generated with gdal2tiles --leaflet ...
	L.tileLayer('./tiles/{z}/{x}/{y}.png', {
		noWrap: true,
		attribution: '',
	}).addTo(map);
}
