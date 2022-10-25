module TeachingMaterial exposing (Model, Msg(..), TeachingResource, config, init, main, teachingResources, update, view)

import TeachingMaterialData exposing (teachingMaterialString)
import MapOfComputionalArchaeology exposing (comparchmap)

import Browser
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Chart.Svg as CS
import Svg as S
import Svg.Attributes as SA
import Html as H exposing (Html, div, h1, input, text, button, p)
import Html.Attributes as HA exposing (placeholder, style)
import Html.Events as HE exposing (onInput)
import Table
import Task
import Csv.Decode as Decode exposing (Decoder)
import List exposing (map)
import VirtualDom exposing (attribute)

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
    -- mapZoom
    , center : CS.Point
    , dragging : Dragging
    , percentage : Float
    }

type Dragging
  = CouldStillBeClick CS.Point
  | ForSureDragging CS.Point
  | None

init : List TeachingResource -> ( Model, Cmd Msg )
init elements =
    let
        model =
            { elements = teachingResources
            , testString = ""
            , tableState = Table.initialSort "Year"
            , query = ""
            -- mapZoom
            , center = { x = 100, y = 50 }
            , dragging = None
            , percentage = 100
            }
    in
    ( model, Cmd.none )

-- UPDATE

type Msg
    = SetQuery String
    | SetTableState Table.State
    -- mapZoom
    | OnMouseDown CS.Point
    | OnMouseMove CS.Point
    | OnMouseUp CS.Point CS.Point
    | OnMouseLeave
    | OnZoomIn
    | OnZoomOut
    | OnZoomReset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        
        SetQuery newQuery ->
            ( { model | query = newQuery }
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        -- mapZoom
        OnMouseDown offset ->
          ({ model | dragging = CouldStillBeClick offset }, Cmd.none)

        OnMouseMove offset ->
          case model.dragging of
            CouldStillBeClick prevOffset ->
              if prevOffset == offset then
                (model, Cmd.none)
              else
                ( { model | center = updateCenter model.center prevOffset offset
                , dragging = ForSureDragging offset
                }, Cmd.none )
            ForSureDragging prevOffset ->
              ( { model | center = updateCenter model.center prevOffset offset
              , dragging = ForSureDragging offset
              }, Cmd.none )
            None ->
              (model, Cmd.none)

        OnMouseUp offset coord ->
          case model.dragging of
            CouldStillBeClick prevOffset ->
              ({ model | center = coord, dragging = None }, Cmd.none)
            ForSureDragging prevOffset ->
              ( { model | center = updateCenter model.center prevOffset offset
              , dragging = None
              }, Cmd.none )
            None ->
              (model, Cmd.none)

        OnMouseLeave ->
          ({ model | dragging = None }, Cmd.none)

        OnZoomIn ->
          ({ model | percentage = model.percentage + 20 }, Cmd.none)

        OnZoomOut ->
          ({ model | percentage = max 1 (model.percentage - 20) }, Cmd.none)

        OnZoomReset ->
          ({ model | percentage = 100, center = { x = 0, y = 0 } }, Cmd.none)


updateCenter : CS.Point -> CS.Point -> CS.Point -> CS.Point
updateCenter center prevOffset offset =
  { x = center.x + (prevOffset.x - offset.x)
  , y = center.y + (prevOffset.y - offset.y)
  }

-- VIEW


view : Model -> Html Msg
view { elements, testString, tableState, query, center, dragging, percentage } =
    let
        lowerQuery =
            String.toLower query

        acceptablePeople =
            List.filter (String.contains lowerQuery << String.toLower << .name) elements

        mapPlot = 
            C.chart
                [ CA.height 270
                , CA.width 492
                , CA.range [ CA.zoom percentage, CA.centerAt center.x ]
                , CA.domain [ CA.zoom percentage, CA.centerAt center.y ]
                , CE.onMouseDown OnMouseDown CE.getOffset
                , CE.onMouseMove OnMouseMove CE.getOffset
                , CE.on "mouseup" (CE.map2 OnMouseUp CE.getOffset CE.getCoords)
                , CE.onMouseLeave OnMouseLeave
                , CA.htmlAttrs
                    [ HA.style "user-select" "none"
                    , HA.style "cursor" <|
                        case dragging of
                          CouldStillBeClick _ -> "grabbing"
                          ForSureDragging _ -> "grabbing"
                          None -> "grab"
                    ]
                ]
                [ C.xLabels [ CA.withGrid, CA.amount 10, CA.ints, CA.fontSize 9 ]
                , C.yLabels [ CA.withGrid, CA.amount 10, CA.ints, CA.fontSize 9 ]
                , C.xTicks [ CA.amount 10, CA.ints ]
                , C.yTicks [ CA.amount 10, CA.ints ]
                , C.htmlAt .max .max 0 0
                    [ HA.style "transform" "translateX(-100%)" ]
                    [ H.span
                        [ HA.style "margin-right" "5px" ]
                        [ H.text (String.fromFloat percentage ++ "%") ]
                    , H.button
                        [ HE.onClick OnZoomIn
                        , HA.style "margin-right" "5px"
                        ]
                        [ H.text "+" ]
                    , H.button
                        [ HE.onClick OnZoomOut
                        , HA.style "margin-right" "5px"
                        ]
                        [ H.text "-" ]
                    , H.button
                        [ HE.onClick OnZoomReset ]
                        [ H.text "⨯" ]
                    ]

                , C.svgAt (\_ -> 0) (\_ -> 100) 0 0 [ comparchmap [
                        attribute "width" (String.fromFloat (600 * (percentage / 100)))
                      , attribute "height" (String.fromFloat (300 * (percentage / 100)))
                      , attribute "viewBox" ("0 0 2000 1000")
                    ] ]
                , C.series .x [ C.scatter .y [] ] elements
                ]
    in
    div []
        [ div [ style "overflow" "hidden", style "margin" "auto", style "width" "40%", style "border" "3px solid green" ] [ mapPlot ]
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


