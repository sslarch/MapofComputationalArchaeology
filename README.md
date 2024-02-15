# MapofComputationalArchaeology

This repo stores code and data for a small web app to list teaching material in the field of Computational Archaeology: https://sslarch.github.io/MapofComputationalArchaeology

The teaching material table is stored in `data/teachingmaterial.tsv` and the web app queries it directly from the master branch on GitHub. **To add new material or modify the existing entries, just edit this file and submit a pull request with your changes.** The file will then be structurally validated by a GitHub action and can be merged if it is sound.

## Developer notes

This web app was written in [Elm](https://elm-lang.org). See the `src` directory for the code and look [here](https://guide.elm-lang.org/install/elm.html) for instructions on how to run and test it locally with `elm reactor`.

### Deploying the app

To prepare the app for deployment on GitHub run `script_build_elmjs.sh`, which will transpile the Elm code in `src` to a single, large `.js` file, and then reduce its size with [uglifyjs](https://github.com/mishoo/UglifyJS) to produce the minified script `elm.min.js`. This file stores the entire app. The `index.html` file finally calls this script. This is what GitHub detects and shows as the website.

### Validating `data/teachingmaterial.tsv`

To allow for convenient, yet safe editing of the input data in `data/teachingmaterial.tsv` an automatic GitHub action workflow runs upon every push to the main branch. This workflow is specified in `.github/workflows/validateTeachingMaterialTable.yml` and involves the following main steps:

1. The Elm compiler and the testing tool elm-test-rs are installed as specified in `elm-tooling.json`.
2. The script `script_teachingmaterial_tsv2elm.sh` is run to create a module `src/TeachingMaterialData.elm` with a string-representation of the table.
3. elm-test-rs triggers the pseudo-test in `tests/Tests.elm`, which runs the parsing code in `src/TeachingMaterial.elm` on the string-representation in `src/TeachingMaterialData.elm`.

If the parsing fails in any way, then the GitHub action will fail, reporting the specific parsing issue. If it does not fail, then the new version of `data/teachingmaterial.tsv` can be read by the app and is safe to merge.

### Changing the fantasy map

The original version of the base map (`data/Archaeologia.map`) was created with this [Fantasy-Map-Generator](https://azgaar.github.io/Fantasy-Map-Generator) and can theoretically still be edited with it. The derived file `data/comparchmap.svg` replaced it, though, for all means and purposes, as several manual steps have to be performed to wrangle the output of the map generator into a .svg file usable with the elm-charts library (simplifying layers, merging objects, replacing labels). Ideally future changes should be performed on the .svg file only. Only for more complex changes (coastlines) it may be necessary to go back to the .map file.

If the `data/comparchmap.svg` is changed then it has to be transformed to Elm code by running `script_map_svg2elm.sh`. Elm can not read the .svg file on the fly. Instead the drawing has to be converted to Elm code with the [svg2elm](https://github.com/pinata-llc/svg2elm) tool which directly creates a dedicated Elm module (`src/MapOfComputionalArchaeology.elm`) for it. This takes a minute, because the .svg file is fairly large.
