module Presidents exposing (Model, Msg(..), Person, config, init, main, presidents, update, view)

import PresidentsData exposing (presidentsString)

import Browser
import Html exposing (Html, div, h1, input, text, button, p)
import Html.Attributes exposing (placeholder)
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
    { people : List Person
    , testString : String
    , tableState : Table.State
    , query : String
    }


init : List Person -> ( Model, Cmd Msg )
init people =
    let
        model =
            { people = presidents
            , testString = presidentsString
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
view { people, testString, tableState, query } =
    let
        lowerQuery =
            String.toLower query

        acceptablePeople =
            List.filter (String.contains lowerQuery << String.toLower << .name) people
    in
    div []
        [ p [] [ text testString ]
        , h1 [] [ text "Birthplaces of U.S. Presidents" ]
        , input [ placeholder "Search by Name", onInput SetQuery ] []
        , Table.view config tableState acceptablePeople
        ]


config : Table.Config Person Msg
config =
    Table.config
        { toId = .name
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Name" .name
            , Table.intColumn "Year" .year
            , Table.stringColumn "City" .city
            , Table.stringColumn "State" .state
            ]
        }



-- PEOPLE

type alias Person =
    { name : String
    , year : Int
    , city : String
    , state : String
    }

decoder : Decoder Person
decoder =
    Decode.into Person
        |> Decode.pipeline (Decode.field "name" Decode.string)
        |> Decode.pipeline (Decode.field "year" Decode.int)
        |> Decode.pipeline (Decode.field "city" Decode.string)
        |> Decode.pipeline (Decode.field "state" Decode.string)

presidents : List Person
presidents =
    case Decode.decodeCsv Decode.FieldNamesFromFirstRow decoder presidentsString of
        Err x -> let 
                    _ = Debug.log " " (Decode.errorToString x)
                 in []
        Ok x -> x


