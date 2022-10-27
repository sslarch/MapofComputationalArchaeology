#!/bin/bash

# teaching material csv to elm code
cp data/teachingmaterial.csv src/TeachingMaterialData.elm
prepent='module TeachingMaterialData exposing (teachingMaterialString)
\n 
\nteachingMaterialString : String
\nteachingMaterialString = """'
echo -en $prepent | cat - src/TeachingMaterialData.elm > temp && mv temp src/TeachingMaterialData.elm
echo '"""' >> src/TeachingMaterialData.elm
