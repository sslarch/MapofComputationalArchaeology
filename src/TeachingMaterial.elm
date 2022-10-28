module TeachingMaterial exposing (Model, Msg(..), TeachingResource, init, main, teachingResources, update, view)

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
import Html as H exposing (Html, div, h1, input, text, button, p, br, span, a)
import Html.Attributes as HA exposing (placeholder, style, href)
import Html.Events as HE exposing (onInput)
import Table
import Task
import Csv.Decode as Decode exposing (Decoder)
import List exposing (map)
import VirtualDom exposing (attribute)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing
import String exposing (split)

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
    , tableState : Table.State
    , query : String
    -- mapZoom
    , center : CS.Point
    , dragging : Dragging
    , percentage : Float
    , hovering : List (CI.One TeachingResource CI.Dot)
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
            , tableState = Table.initialSort "ID"
            , query = ""
            -- mapZoom
            , center = { x = 100, y = 50 }
            , dragging = None
            , percentage = 100
            , hovering = []
            }
    in
    ( model, Cmd.none )

-- UPDATE

type Msg
    = SetQuery String
    | SetTableState Table.State
    -- map
    | OnMouseDown CS.Point
    | OnMouseMove CS.Point (List (CI.One TeachingResource CI.Dot))
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

        -- map
        OnMouseDown offset ->
          ({ model | dragging = CouldStillBeClick offset }, Cmd.none)

        OnMouseMove offset hovering ->
          case model.dragging of
            CouldStillBeClick prevOffset ->
              if prevOffset == offset then
                ({ model | hovering = hovering }, Cmd.none)
              else
                ( { model | center = updateCenter model.center prevOffset offset
                , dragging = ForSureDragging offset
                , hovering = hovering
                }, Cmd.none )
            ForSureDragging prevOffset ->
              ( { model | center = updateCenter model.center prevOffset offset
              , dragging = ForSureDragging offset
              , hovering = hovering
              }, Cmd.none )
            None ->
              ({ model | hovering = hovering }, Cmd.none)

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
          ({ model | dragging = None, hovering = [] }, Cmd.none)

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
view { elements, tableState, query, center, dragging, percentage, hovering } =
    let
        -- MAP PLOT
        
        findValueByCoordinates x y =
            List.head <| List.filter (\e -> e.x == x && e.y == y) elements

        mapPlot = 
            C.chart
                [ CA.height 270
                , CA.width 492
                , CA.range [ CA.zoom percentage, CA.centerAt center.x ]
                , CA.domain [ CA.zoom percentage, CA.centerAt center.y ]
                , CE.onMouseDown OnMouseDown CE.getOffset
                , CE.on "mousemove" (CE.map2 OnMouseMove CE.getOffset (CE.getNearest CI.dots))
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
                [ 
                -- uncomment to get a coordinate grid
                --C.xLabels [ CA.withGrid, CA.amount 10, CA.ints, CA.fontSize 9 ]
                --, C.yLabels [ CA.withGrid, CA.amount 10, CA.ints, CA.fontSize 9 ]
                --, C.xTicks [ CA.withGrid, CA.amount 10, CA.ints ]
                --, C.yTicks [ CA.withGrid, CA.amount 10, CA.ints ],
                 C.htmlAt .max .max -5 -5
                    [ HA.style "transform" "translateX(-100%)" ]
                    [ H.span
                        [ HA.style "margin-right" "5px" ]
                        [ H.text (String.fromFloat percentage ++ "%") ]
                    , Button.button
                        [ Button.attrs [ HE.onClick OnZoomIn, Spacing.ml1 ]
                        , Button.outlineSecondary
                        , Button.small
                        ]
                        [ H.text "+" ]
                    , Button.button
                        [ Button.attrs [ HE.onClick OnZoomOut, Spacing.ml1 ]
                        , Button.outlineSecondary
                        , Button.small
                        ]
                        [ H.text "-" ]
                    , Button.button
                        [ Button.attrs [ HE.onClick OnZoomReset, Spacing.ml1 ]
                        , Button.outlineSecondary
                        , Button.small
                        ]
                        [ H.text "⨯" ]
                    ]

                , C.svgAt (\_ -> 0) (\_ -> 100) 0 0 [ comparchmap [
                        attribute "width" (String.fromFloat (600 * (percentage / 100)))
                      , attribute "height" (String.fromFloat (300 * (percentage / 100)))
                      , attribute "viewBox" ("0 0 2000 1000")
                    ] ]
                , C.series .x [ C.scatter .y [ CA.color CA.red, CA.size 2, CA.square ] |> C.named "Teaching resource" ] elements
                , C.each hovering <| \p item -> [ C.tooltip item [ 
                      CA.offset 0
                    ] [] [
                      case (findValueByCoordinates (CI.getX item) (CI.getY item)) of
                        Nothing -> H.text ""
                        Just x -> H.text (x.id ++ ": " ++ x.name)
                    ] ]
                ]

        -- TABLE

        lowerQuery =
            String.toLower query

        acceptablePeople =
            List.filter (String.contains lowerQuery << String.toLower << .name) elements

        idColumn : String -> (data -> String) -> Table.Column data msg
        idColumn colName toString =
          Table.veryCustomColumn
            { name = colName
            , viewData = \data -> (\s -> Table.HtmlDetails [] [ span [
                  style "display" "inline-block"
                , style "font-weight" "bold"
                , style "padding" "5px"
                ] [text s] ]) (toString data)
            , sorter = Table.increasingOrDecreasingBy toString
            }

        nameColumn : String -> (data -> String) -> Table.Column data msg
        nameColumn colName toString =
          Table.veryCustomColumn
            { name = colName
            , viewData = \data -> (\s -> Table.HtmlDetails [] [ span [ 
                  style "display" "inline-block"
                , style "font-style" "italic"
                , style "padding" "5px"
                ] [text s] ]) (toString data)
            , sorter = Table.unsortable
            }

        viewAuthor : List String -> Table.HtmlDetails msg
        viewAuthor ss =
            let 
                firstAuthor = List.head ss
                moreThanOneAuthor = List.length ss > 1
            in
                case firstAuthor of
                    Nothing -> Table.HtmlDetails [] [ text "" ]
                    Just a ->  Table.HtmlDetails [] [ text (if moreThanOneAuthor then (a ++ " et al.") else a) ]

        badgeStyle = 
            [ 
              style "display" "inline-block"
            , style "color" "white"
            , style "padding" "1px 4px"
            , style "text-align" "center"
            , style "border-radius" "5px"
            , style "margin" "3px"
            ]

        viewProgrammingLanguage : List String -> Table.HtmlDetails msg
        viewProgrammingLanguage ss =
            Table.HtmlDetails [] (map (\s -> span (badgeStyle ++ [ style "background-color" "#80b3ffff" ]) [ text s ]) ss)

        viewTags : List String -> Table.HtmlDetails msg
        viewTags ss =
            Table.HtmlDetails [] (map (\s -> span (badgeStyle ++ [ style "background-color" "#bfb891ff" ]) [ text s ]) ss)

        stringListColumn : String -> (data -> List String) -> (List String -> Table.HtmlDetails msg) -> Table.Column data msg
        stringListColumn colName toStringList viewFunction =
          Table.veryCustomColumn
            { name = colName
            , viewData = \data -> viewFunction (toStringList data)
            , sorter = Table.unsortable
            }

        linkColumn : String -> (data -> String) -> Table.Column data msg
        linkColumn colName toString =
          Table.veryCustomColumn
            { name = colName
            , viewData = \data -> (\s -> Table.HtmlDetails [] [ a [ href s ] [ text "Source" ] ]) (toString data)
            , sorter = Table.unsortable
            }

        tableConfig : Table.Config TeachingResource Msg
        tableConfig =
            Table.config
                { toId = .id
                , toMsg = SetTableState
                , columns =
                    [ 
                      idColumn "ID" .id
                    , Table.stringColumn "Year" .year
                    , nameColumn "Resource name" .name
                    , stringListColumn "Author" .author viewAuthor
                    , stringListColumn "Code" .programmingLanguage viewProgrammingLanguage
                    , stringListColumn "Tags" .tags viewTags
                    , linkColumn "Link" .link
                    ]
                }

    in
        -- PAGE LAYOUT
        Grid.container [] [
              CDN.stylesheet -- Don't use this method when you want to deploy your app for real life usage. http://elm-bootstrap.info/getting-started
            , Grid.row [] [
                  Grid.col [ Col.sm12 ] 
                    [ 
                      div [ 
                          style "overflow" "hidden"
                        , style "margin" "auto"
                        , style "height" "380px"
                        , style "width" "100%"
                        ] [ mapPlot ] 
                    ]
                ]
            , Grid.row [] [
                Grid.col [ ]
                    [
                      br [] []
                    , h1 [] [ text "Teaching material list" ]
                    , Alert.simpleDark [] [ text "Explore the list: ",  input [ placeholder "Search by Name", onInput SetQuery ] [] ]
                    , Table.view tableConfig tableState acceptablePeople
                    ]
                ]
            ]

-- DATA

type alias TeachingResource =
    { id : String
    , name : String
    , author : List String
    , year : String
    , topic : String
    , language : String
    , programmingLanguage : List String
    , tools : List String
    , levelOfDifficulty : List String
    , description : String
    , materialType : String
    , tags : List String
    , tagsOpenArchaeo : List String
    , link : String
    , citation : String
    , x : Float
    , y : Float
    }

decoder : Decoder TeachingResource
decoder =
    let
        decodeStringList = Decode.map (\s -> split "," s) Decode.string
    in
        Decode.into TeachingResource
            |> Decode.pipeline (Decode.field "ID" Decode.string)
            |> Decode.pipeline (Decode.field "Name" Decode.string)
            |> Decode.pipeline (Decode.field "Author" decodeStringList)
            |> Decode.pipeline (Decode.field "Year" Decode.string)
            |> Decode.pipeline (Decode.field "Topic" Decode.string)
            |> Decode.pipeline (Decode.field "Language" Decode.string)
            |> Decode.pipeline (Decode.field "Programming_language" decodeStringList)
            |> Decode.pipeline (Decode.field "Tools" decodeStringList)
            |> Decode.pipeline (Decode.field "Level_of_difficulty" decodeStringList)
            |> Decode.pipeline (Decode.field "Description" Decode.string)
            |> Decode.pipeline (Decode.field "Material_type" Decode.string)
            |> Decode.pipeline (Decode.field "Tags" decodeStringList)
            |> Decode.pipeline (Decode.field "Tags_openarchaeo" decodeStringList)
            |> Decode.pipeline (Decode.field "Link" Decode.string)
            |> Decode.pipeline (Decode.field "Citation" Decode.string)
            |> Decode.pipeline (Decode.field "X_map" Decode.float)
            |> Decode.pipeline (Decode.field "Y_map" Decode.float)

teachingResources : List TeachingResource
teachingResources =
    case Decode.decodeCustom {fieldSeparator = '\t'} Decode.FieldNamesFromFirstRow decoder teachingMaterialString of
        Err x -> --let _ = Debug.log "Error when parsing input data" (Decode.errorToString x)
                 --in []
                 []
        Ok x -> x


