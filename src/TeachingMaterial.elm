module TeachingMaterial exposing (
    TeachingResource, Difficulty (..),
    makeDummyResource, difficultyToString, parseTeachingResources)

import Csv.Decode as Decode exposing (Decoder)
import String exposing (split, trim)

type alias TeachingResource =
    { id                        : String
    , source                    : List String
    , x                         : Float
    , y                         : Float
    , name                      : String
    , author                    : List String
    , year                      : String
    , topic                     : String
    , language                  : String
    , programmingLanguage       : List String
    , tools                     : List String
    , levelOfDifficulty         : Difficulty
    , description               : String
    , materialType              : String
    , tags                      : List String
    , tagsOpenArchaeo           : List String
    , link                      : String
    , citation                  : String
    }

type Difficulty =
      Beginner
    | Intermediate
    | Advanced

difficultyFromString : String -> Result String Difficulty
difficultyFromString s = case s of
    "beginner"      -> Ok Beginner
    "intermediate"  -> Ok Intermediate
    "advanced"      -> Ok Advanced
    _               -> Err "invalid difficulty string"

difficultyToString : Difficulty -> String
difficultyToString d = case d of
    Beginner        -> "beginner"
    Intermediate    -> "intermediate"
    Advanced        -> "advanced"

makeDummyResource : Float -> Float -> TeachingResource
makeDummyResource x y = {
      id = ""
    , source = []
    , x = x
    , y = y
    , name = ""
    , author = []
    , year = ""
    , topic = ""
    , language = ""
    , programmingLanguage = []
    , tools = []
    , levelOfDifficulty = Beginner
    , description = ""
    , materialType = ""
    , tags = []
    , tagsOpenArchaeo = []
    , link = ""
    , citation = ""
    }

decodeTeachingResource : Decoder TeachingResource
decodeTeachingResource =
    let decodeStringList = Decode.map (List.map trim) <| Decode.map (\s -> split "," s) Decode.string
        decodeStringListLower = decodeStringList |> Decode.map (List.map String.toLower)
        decodeDifficulty = Decode.string |>
                           Decode.andThen (\value -> Decode.fromResult (difficultyFromString value))
    in Decode.into TeachingResource
            |> Decode.pipeline (Decode.field "ID" Decode.string)
            |> Decode.pipeline (Decode.field "Source" decodeStringList)
            |> Decode.pipeline (Decode.field "X_map" Decode.float)
            |> Decode.pipeline (Decode.field "Y_map" Decode.float)
            |> Decode.pipeline (Decode.field "Name" Decode.string)
            |> Decode.pipeline (Decode.field "Author" decodeStringList)
            |> Decode.pipeline (Decode.field "Year" Decode.string)
            |> Decode.pipeline (Decode.field "Topic" Decode.string)
            |> Decode.pipeline (Decode.field "Language" Decode.string)
            |> Decode.pipeline (Decode.field "Programming_language" decodeStringListLower)
            |> Decode.pipeline (Decode.field "Tools" decodeStringList)
            |> Decode.pipeline (Decode.field "Level_of_difficulty" decodeDifficulty)
            |> Decode.pipeline (Decode.field "Description" Decode.string)
            |> Decode.pipeline (Decode.field "Material_type" Decode.string)
            |> Decode.pipeline (Decode.field "Tags" decodeStringListLower)
            |> Decode.pipeline (Decode.field "Tags_openarchaeo" decodeStringList)
            |> Decode.pipeline (Decode.field "Link" Decode.string)
            |> Decode.pipeline (Decode.field "Citation" Decode.string)

parseTeachingResources : String -> List TeachingResource
parseTeachingResources teachingMaterialString =
    case Decode.decodeCustom {fieldSeparator = '\t'} Decode.FieldNamesFromFirstRow decodeTeachingResource teachingMaterialString of
        Err x -> -- for debugging of .tsv file and parsing
                 --let _ = Debug.log "Parsing error" (Decode.errorToString x)
                 --in []
                 []
        Ok x -> x
