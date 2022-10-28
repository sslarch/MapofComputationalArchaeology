# MapofComputationalArchaeology

## Data

Raw data lives in `data`.

The original version of the base map (`data/Archaeologia.map`) was created with this [Fantasy-Map-Generator](https://azgaar.github.io/Fantasy-Map-Generator) and can theoretically still be edited with it. The derived file `data/comparchmap.svg` replaced it, though, for all means and purposes, as several manual steps have to be performed to wrangle the output of the map generator into a .svg file usable with the elm-charts library (simplifying layers, merging objects, replacing labels). Ideally future changes should be performed on the .svg file only. For more complex changes (coastlines) it may be necessary to go back to the .map file.

Elm can not read the .svg file on the fly. Instead the drawing has to be converted to Elm code with the [svg2elm](https://github.com/pinata-llc/svg2elm) tool which directly creates a dedicated Elm module for it. Running `script_map_svg2elm.sh` executes the relevant bash command. This takes a minute, because the .svg file is fairly large. The resulting module `src/MapOfComputionalArchaeology.elm` is a derived data product and therefore not logged with Git to safe space.

The teaching material table is stored in `data/teachingmaterial.tsv`. Just as the map is has to be translated to Elm code to make it accessible for the webapp. The script `script_teachingmaterial_tsv2elm.sh` creates a module `src/TeachingMaterialData.elm` with a string ready to be parsed. Just as the map module this is a derived data product and not logged by Git.

## Elm

The webapp was written in [Elm](https://elm-lang.org/) and exists in a single file `src/Main.elm`. See the instructions [here](https://guide.elm-lang.org/install/elm.html) on how to run and explore the app with `elm reactor`.

To prepare the app for deployment on GitHub run `script_build_elmjs.sh`, which will transpile the Elm code in `src` to a single, large `.js` file and then reduce its size with [uglifyjs](https://github.com/mishoo/UglifyJS) to produce the script `elm.min.js`. This file stores the entire app with all data and code. The `index.html` file finally runs this script. This is what GitHub detects and shows as the website.
