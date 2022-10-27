#!/bin/bash

# teaching material tsv to elm code
cp data/teachingmaterial.tsv src/TeachingMaterialData.elm
prepent='module TeachingMaterialData exposing (teachingMaterialString)
\n 
\nteachingMaterialString : String
\nteachingMaterialString = """'
echo -en $prepent | cat - src/TeachingMaterialData.elm > temp && mv temp src/TeachingMaterialData.elm
echo '"""' >> src/TeachingMaterialData.elm
