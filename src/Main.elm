module Main exposing (..)

import TeachingMaterialData exposing (teachingMaterialString)
import MapOfComputionalArchaeology exposing (comparchmap)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Modal as Modal
import Browser
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Chart.Svg as CS
import Csv.Decode as Decode exposing (Decoder)
import FontAwesome as Icon exposing (Icon)
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Html as H exposing (Html, div, h1, input, text, button, p, br, span, a)
import Html.Attributes as HA exposing (placeholder, style, href)
import Html.Events as HE exposing (onInput)
import List exposing (map)
import String exposing (split)
import Svg as S
import Svg.Attributes as SA
import Table
import Task
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
    , tableState : Table.State
    , nameQuery : String
    , programmingLanguageQuery : String
    , tagsQuery : String
    -- mapZoom
    , center : CS.Point
    , dragging : Dragging
    , percentage : Float
    , hovering : List (CI.One TeachingResource CI.Dot)
    -- modal
    , modalVisibility : Modal.Visibility
    , selectedElement : Maybe TeachingResource
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
            , nameQuery = ""
            , programmingLanguageQuery = ""
            , tagsQuery = ""
            -- mapZoom
            , center = { x = 100, y = 50 }
            , dragging = None
            , percentage = 100
            , hovering = []
            -- modal
            , modalVisibility = Modal.hidden
            , selectedElement = Nothing
            }
    in
        ( model, Cmd.none )

-- UPDATE

type Msg
    = SetNameQuery String
    | SetProgrammingLanguageQuery String
    | SetTagsQuery String
    | SetTableState Table.State
    -- map
    | OnMouseDown CS.Point
    | OnMouseMove CS.Point (List (CI.One TeachingResource CI.Dot))
    | OnMouseUp CS.Point CS.Point
    | OnMouseLeave
    | OnZoomIn
    | OnZoomOut
    | OnZoomReset
    -- modal
    | CloseModal
    | ShowModal (Maybe TeachingResource)

updateCenter : CS.Point -> CS.Point -> CS.Point -> CS.Point
updateCenter center prevOffset offset =
  { x = center.x + (prevOffset.x - offset.x)
  , y = center.y + (prevOffset.y - offset.y)
  }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
          ({ model | percentage = 100, center = { x = 100, y = 50 } }, Cmd.none)
        -- table
        SetNameQuery newQuery ->
            ( { model | nameQuery = newQuery }
            , Cmd.none )
        SetProgrammingLanguageQuery newQuery ->
            ( { model | programmingLanguageQuery = newQuery }
            , Cmd.none )
        SetTagsQuery newQuery ->
            ( { model | tagsQuery = newQuery }
            , Cmd.none )
        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none )
        -- modal
        CloseModal ->
            ( { model | modalVisibility = Modal.hidden } , Cmd.none )
        ShowModal element ->
            ( { model | modalVisibility = Modal.shown, selectedElement = element } , Cmd.none )

-- VIEW

view : Model -> Html Msg
view { elements, tableState, nameQuery, programmingLanguageQuery, tagsQuery, center, dragging, percentage, hovering, modalVisibility, selectedElement } =
    let
        -- helpers
        findElementByCoordinates x y =
            List.head <| List.filter (\e -> e.x == x && e.y == y) elements
        findElementByID i =
            List.head <| List.filter (\e -> e.id == i) elements

        -- map
        mapPlot = 
            C.chart
                [ CA.height 294
                , CA.width 590
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
                C.xLabels [ CA.withGrid, CA.amount 10, CA.ints, CA.fontSize 9 ]
                , C.yLabels [ CA.withGrid, CA.amount 10, CA.ints, CA.fontSize 9 ]
                , C.xTicks [ CA.withGrid, CA.amount 10, CA.ints ]
                , C.yTicks [ CA.withGrid, CA.amount 10, CA.ints ],
                 C.htmlAt .max .max -5 -5
                    [ HA.style "transform" "translateX(-100%)" ]
                    [ span
                        [ HA.style "margin-right" "5px" ]
                        [ text (String.fromFloat percentage ++ "%") ]
                    , Button.button
                        [ Button.attrs [ HE.onClick OnZoomIn, Spacing.ml1 ]
                        , Button.outlineSecondary
                        , Button.small
                        ]
                        [ text "+" ]
                    , Button.button
                        [ Button.attrs [ HE.onClick OnZoomOut, Spacing.ml1 ]
                        , Button.outlineSecondary
                        , Button.small
                        ]
                        [ text "-" ]
                    , Button.button
                        [ Button.attrs [ HE.onClick OnZoomReset, Spacing.ml1 ]
                        , Button.outlineSecondary
                        , Button.small
                        ]
                        [ text "⨯" ]
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
                      case (findElementByCoordinates (CI.getX item) (CI.getY item)) of
                        Nothing -> text ""
                        Just x -> text (x.id ++ ": " ++ x.name)
                    ] ]
                ]

        -- search/filter
        lowerNameQuery = String.toLower nameQuery
        lowerProgrammingQuery = String.toLower programmingLanguageQuery
        lowerTagsQuery = String.toLower tagsQuery
        acceptableResources = List.filter (
                (\x ->
                  let 
                      matchName = String.contains lowerNameQuery <| String.toLower <| (x.name ++ String.join "" x.author)
                      matchProg = String.contains lowerProgrammingQuery <| String.toLower <| String.join "" <| x.programmingLanguage
                      matchTag = String.contains lowerTagsQuery <| String.toLower <| String.join "" <| x.tags
                  in matchName && matchProg && matchTag
                )
            ) elements

        -- table
        idColumn : String -> (data -> String) -> Table.Column data msg
        idColumn colName toString =
          Table.veryCustomColumn
            { name = colName
            , viewData = \data -> (\s -> Table.HtmlDetails [] [ span [
                  style "display" "inline-block"
                , style "padding-right" "5px"
                , style "font-size" "12px"
                , style "font-style" "italic"
                , style "transform" "rotate(45deg)"
                ] [text s] ]) (toString data)
            , sorter = Table.increasingOrDecreasingBy toString
            }

        resourceColumn : String -> (data -> String) -> (data -> String) -> (data -> List String) -> Table.Column data msg
        resourceColumn colName getYear getName getAuthors =
          Table.veryCustomColumn
            { name = colName
            , viewData = \data -> (\y n a -> Table.HtmlDetails [] [
                    div [ style "display" "inline-block", style "padding-right" "5px", style "font-size" "16px" ] [
                        span [                  
                          style "font-weight" "bold"
                        ] [ text n ],
                        br [] [] ,
                        text "by ",
                        span [                  
                          style "font-style" "italic"
                        ] [ text a ],
                        text ", ",
                        span [
                          style "font-style" "italic"
                        , style "text-decoration" "underline"
                        ] [ text y ]
                    ] 
                ]) (getYear data) (getName data) (renderAuthors <| getAuthors data)
            , sorter = Table.increasingOrDecreasingBy getYear
            }

        renderAuthors : List String -> String
        renderAuthors ss =
            let 
                firstAuthor = List.head ss
                moreThanOneAuthor = List.length ss > 1
            in
                case firstAuthor of
                    Nothing -> ""
                    Just a ->  if moreThanOneAuthor then (a ++ " et al.") else a

        badgeStyle = 
            [ 
              style "display" "inline-block"
            , style "color" "white"
            , style "padding" "1px 4px"
            , style "text-align" "center"
            , style "border-radius" "5px"
            , style "margin" "2px"
            , style "font-size" "15px"
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

        linkAndModalColumn : String -> (data -> String) -> (data -> String) -> Table.Column data Msg
        linkAndModalColumn colName getLink getID =
          Table.veryCustomColumn
            { name = colName
            , viewData = \data -> (\s -> Table.HtmlDetails [] [
                div [] [
                      Button.linkButton [
                        Button.small, Button.block, Button.outlineSecondary
                      , Button.attrs [ href s, style "margin-bottom" "-5px" ]
                      ] [ Icon.view Icon.link ]
                    , Button.button [ 
                        Button.small, Button.block, Button.outlineSecondary
                      , Button.attrs [ HE.onClick <| ShowModal (findElementByID (getID data)), style "margin-bottom" "10px" ]
                      ] [ Icon.view Icon.infoCircle ] 
                    ]
                ]) (getLink data)
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
                    , resourceColumn "Material" .year .name .author
                    , stringListColumn "Language" .programmingLanguage viewProgrammingLanguage
                    , stringListColumn "Tags" .tags viewTags
                    , linkAndModalColumn "" .link .id
                    ]
                }

        -- modal
        oneRow name value = Grid.row [ ] [ Grid.col [ Col.sm3 ] [ span [ style "font-weight" "bold" ] [ text name ] ] , Grid.col [ Col.sm9 ] [ text value ] ]

        detailsModal =
            case selectedElement of
                Nothing -> 
                    Modal.config CloseModal
                        |> Modal.small
                        |> Modal.hideOnBackdropClick True
                        |> Modal.h3 [] [ text "Error - this should never happen" ]
                        |> Modal.view modalVisibility
                Just x -> 
                    Modal.config CloseModal
                        |> Modal.large
                        |> Modal.hideOnBackdropClick True
                        |> Modal.scrollableBody True
                        |> Modal.h3 [] [ text x.id ]
                        |> Modal.body [] [ 
                            Grid.containerFluid [ ] [
                                  oneRow "Name: "             x.name
                                , oneRow "Author: "           <| String.join ", " x.author
                                , oneRow "Year: "             x.year
                                , oneRow "Topic: "            x.topic
                                , oneRow "Description: "      x.description
                                , oneRow "Language: "         x.language
                                , oneRow "Prog. language: "   <| String.join ", " x.programmingLanguage
                                , oneRow "Tools: "            <| String.join ", " x.tools
                                , oneRow "Level: "            <| String.join ", " x.levelOfDifficulty
                                , oneRow "Material type: "    x.materialType
                                , oneRow "Tags: "             <| String.join ", " x.tags
                                , oneRow "Tags OpenArchaeo: " <| String.join ", " x.tagsOpenArchaeo
                                , oneRow "Link: "             x.link
                                , oneRow "Citation: "         x.citation
                                ]
                            ]
                        |> Modal.footer []
                            [ Button.button
                                [ Button.outlineSecondary
                                , Button.attrs [ HE.onClick CloseModal ]
                                ]
                                [ text "Close" ]
                            ]
                        |> Modal.view modalVisibility


    in
        -- main layout
        div [] [
            Grid.container [] [
                  --CDN.stylesheet, -- Don't use this method when you want to deploy your app for real life usage. http://elm-bootstrap.info/getting-started
                  Icon.css -- Fontawesome
                , Grid.row [] [
                      Grid.col [ Col.sm12 ] 
                        [ 
                          div [ 
                              style "overflow" "hidden"
                            , style "margin" "auto"
                            , style "height" "100%"
                            , style "width" "100%"
                            ] [ mapPlot ] 
                        ]
                    ]
                , Grid.row [] [
                    Grid.col [ ]
                        [
                          br [] []
                        , div [] [
                                span [ style "font-size" "35px" ] [ text "Computational archaeology teaching material list" ]
                              , span [ style "display" "inline-block", style "width" "20px" ] []
                              , text " a project by the "
                              , a [ href "https://sslarch.github.io" ] [ text "SIG SSLA" ]
                            ]
                        , Alert.simpleDark [] [
                            Grid.container []
                                [ Grid.row [ Row.centerMd ]
                                    [ Grid.col [ Col.xs12, Col.mdAuto ] [ text "Filter the list:" ]
                                    , Grid.col [ Col.xs12, Col.mdAuto ] [ 
                                            input [ style "margin" "2px", placeholder "by Name and Authors", onInput SetNameQuery ] [] 
                                        ]
                                    , Grid.col [ Col.xs12, Col.mdAuto ] [
                                            input [ style "margin" "2px", placeholder "by Language", onInput SetProgrammingLanguageQuery ] []
                                        ]
                                    , Grid.col [ Col.xs12, Col.mdAuto ] [
                                            input [ style "margin" "2px", placeholder "by Tag", onInput SetTagsQuery ] []
                                        ]
                                    ]
                                ]
                            ]
                        , Table.view tableConfig tableState acceptableResources
                        ]
                    ]
                ]
            , detailsModal
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
        Err x -> --let _ = Debug.log "Error when parsing input data" (Decode.errorToString x) -- for debugging of .tsv file
                 --in []
                 []
        Ok x -> x