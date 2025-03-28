module TeachingMaterial exposing (
    TeachingResource, Difficulty (..),
    makeDummyResource, difficultyToString, parseTeachingResources)

import Yaml.Decode as Decode exposing (Decoder)
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
    in Decode.succeed TeachingResource
            |> Decode.andMap (Decode.field "ID" Decode.string)
            |> Decode.andMap (Decode.field "Source" decodeStringList)
            |> Decode.andMap (Decode.field "X_map" Decode.float)
            |> Decode.andMap (Decode.field "Y_map" Decode.float)
            |> Decode.andMap (Decode.field "Name" Decode.string)
            |> Decode.andMap (Decode.field "Author" decodeStringList)
            |> Decode.andMap (Decode.field "Year" Decode.string)
            |> Decode.andMap (Decode.field "Topic" Decode.string)
            |> Decode.andMap (Decode.field "Language" Decode.string)
            |> Decode.andMap (Decode.field "Programming_language" decodeStringListLower)
            |> Decode.andMap (Decode.field "Tools" decodeStringList)
            |> Decode.andMap (Decode.field "Level_of_difficulty" decodeDifficulty)
            |> Decode.andMap (Decode.field "Description" Decode.string)
            |> Decode.andMap (Decode.field "Material_type" Decode.string)
            |> Decode.andMap (Decode.field "Tags" decodeStringListLower)
            |> Decode.andMap (Decode.field "Tags_openarchaeo" decodeStringList)
            |> Decode.andMap (Decode.field "Link" Decode.string)
            |> Decode.andMap (Decode.field "Citation" Decode.string)

parseTeachingResources : String -> Result String (List TeachingResource)
parseTeachingResources teachingMaterialString =
    case Decode.fromString (Decode.list decodeTeachingResource) teachingMaterialString of
        Err e -> Err (Decode.errorToString e)
        Ok x ->  Ok x
