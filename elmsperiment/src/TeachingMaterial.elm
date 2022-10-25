module TeachingMaterial exposing (Model, Msg(..), TeachingResource, config, init, main, teachingResources, update, view)

import TeachingMaterialData exposing (teachingMaterialString)
import MapOfComputionalArchaeology exposing (comparchmap)

import Browser
import Chart as C
import Chart.Attributes as CA
import Chart.Svg as CS
import Svg as S
import Svg.Attributes as SA
import Html exposing (Html, div, h1, input, text, button, p)
import Html.Attributes exposing (placeholder, style)
import Html.Events exposing (onInput)
import Table
import Task
import Csv.Decode as Decode exposing (Decoder)
import List exposing (map)

main =
    Browser.element
        { init = \() -> init []
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { elements : List TeachingResource
    , testString : String
    , tableState : Table.State
    , query : String
    }


init : List TeachingResource -> ( Model, Cmd Msg )
init elements =
    let
        model =
            { elements = teachingResources
            , testString = ""
            , tableState = Table.initialSort "Year"
            , query = ""
            }
    in
    ( model, Cmd.none )



-- UPDATE


type Msg
    = SetQuery String
    | SetTableState Table.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        
--        LoadCsvFile newList ->
--           ( { model | people = newList }
--           , Cmd.none 
--           )

        SetQuery newQuery ->
            ( { model | query = newQuery }
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view { elements, testString, tableState, query } =
    let
        lowerQuery =
            String.toLower query

        acceptablePeople =
            List.filter (String.contains lowerQuery << String.toLower << .name) elements

        mapPlot = 
            C.chart
                [ CA.height 300
                , CA.width 600
                ]
                [ C.xLabels [ CA.withGrid ]
                , C.yLabels [ CA.withGrid ]
                , C.svgAt .min .max 0 0 [ comparchmap [] ]
                , C.series .x [ C.scatter .y [] ] elements
                ]
    in
    div []
        [ div [ style "margin" "auto", style "width" "50%", style "border" "3px solid green" ] [ mapPlot ]
        , h1 [] [ text "Teaching material list" ]
        , input [ placeholder "Search by Name", onInput SetQuery ] []
        , Table.view config tableState acceptablePeople
        ]


config : Table.Config TeachingResource Msg
config =
    Table.config
        { toId = .name
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Name" .name
            , Table.stringColumn "Author" .author
            , Table.stringColumn "Year" .year
            , Table.stringColumn "Topic" .topic
            ]
        }



-- PEOPLE

type alias TeachingResource =
    { x : Float
    , y : Float
    , name : String
    , author : String
    , year : String
    , topic : String
    }

decoder : Decoder TeachingResource
decoder =
    Decode.into TeachingResource
        |> Decode.pipeline (Decode.field "X" Decode.float)
        |> Decode.pipeline (Decode.field "Y" Decode.float)
        |> Decode.pipeline (Decode.field "Name" Decode.string)
        |> Decode.pipeline (Decode.field "Author" Decode.string)
        |> Decode.pipeline (Decode.field "Year" Decode.string)
        |> Decode.pipeline (Decode.field "Topic" Decode.string)

teachingResources : List TeachingResource
teachingResources =
    case Decode.decodeCustom {fieldSeparator = '\t'} Decode.FieldNamesFromFirstRow decoder teachingMaterialString of
        Err x -> let _ = Debug.log "Error when parsing input data" (Decode.errorToString x)
                 in []
        Ok x -> x


