#!/bin/bash

cp data/presidents.csv src/PresidentsData.elm
prepent='module PresidentsData exposing (presidentsString)
\n 
\npresidentsString : String
\npresidentsString = """'
echo -en $prepent | cat - src/PresidentsData.elm > temp && mv temp src/PresidentsData.elm
echo '"""' >> src/PresidentsData.elm