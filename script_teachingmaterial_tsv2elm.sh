#!/bin/bash

# teaching material tsv to elm code
cp data/teachingmaterial.tsv src/TeachingMaterialData.elm
prepent1='module TeachingMaterialData exposing (teachingMaterialString)
\n 
\nteachingMaterialString : String
\nteachingMaterialString = """'
echo -en $prepent1 | cat - src/TeachingMaterialData.elm > temp && mv temp src/TeachingMaterialData.elm
echo '"""' >> src/TeachingMaterialData.elm

# teaching material edgelist tsv to elm code
cp data/edgelist_teachingmaterial.tsv src/TeachingMaterialEdges.elm
prepent2='module TeachingMaterialData exposing (teachingMaterialEdgesString)
\n 
\nteachingMaterialEdgesString : String
\nteachingMaterialEdgesString = """'
echo -en $prepent2 | cat - src/TeachingMaterialEdges.elm > temp && mv temp src/TeachingMaterialEdges.elm
echo '"""' >> src/TeachingMaterialEdges.elm
