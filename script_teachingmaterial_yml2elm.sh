#!/bin/bash

# teaching material yml to elm code
cp data/teachingmaterial.yml src/TeachingMaterialData.elm
prepent1='module TeachingMaterialData exposing (teachingMaterialString)
\n 
\nteachingMaterialString : String
\nteachingMaterialString = """'
echo -en $prepent1 | cat - src/TeachingMaterialData.elm > temp && mv temp src/TeachingMaterialData.elm
echo '"""' >> src/TeachingMaterialData.elm
