module Main exposing (..)

import MapOfComputionalArchaeology exposing (comparchmap)
import TeachingMaterialData exposing (teachingMaterialString)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Browser.Events as E
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Chart.Svg as CS
import Csv.Decode as Decode exposing (Decoder)
import FontAwesome as Icon exposing (Icon)
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Html as H exposing (Html, a, br, button, div, h1, input, p, span, text)
import Html.Attributes as HA exposing (href, placeholder, style)
import Html.Events as HE exposing (onInput)
import List exposing (map)
import Maybe.Extra exposing (values)
import String exposing (split, trim)
import Svg as S
import Svg.Attributes as SA
import Table exposing (defaultCustomizations)
import Task
import VirtualDom exposing (attribute)

-- MAIN

-- for the test environment with elm-reactor (open Reactor.elm!)
reactor =
    Browser.element
        { init = \() -> init 500 []
        , update = update
        , view = view True -- devel mode = True
        , subscriptions = subscriptions
        }

-- for production and deployment
main =
    Browser.element
        { init = \wW -> init wW []
        , update = update
        , view = view False -- devel mode = False
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model = {
      windowWidth               : Int
    -- table and data
    , elements                  : List TeachingResource
    , tableState                : Table.State
    , nameQuery                 : String
    , programmingLanguageQuery  : String
    , tagsQuery                 : String
    -- map
    , center                    : CS.Point
    , dragging                  : Dragging
    , percentage                : Float
    , hovering                  : List (CI.One TeachingResource CI.Dot)
    , clickedElement            : Maybe TeachingResource
    -- modal
    , modalVisibility           : Modal.Visibility
    , selectedElement           : Maybe TeachingResource
    -- welcome
    , welcomeVisibility         : Modal.Visibility
    }

type Dragging =
    CouldStillBeClick CS.Point
  | ForSureDragging CS.Point
  | None

-- DATA

type alias TeachingResource =
    { id                        : String
    , partner                   : List String
    , x                         : Float
    , y                         : Float
    , name                      : String
    , author                    : List String
    , year                      : String
    , topic                     : String
    , language                  : String
    , programmingLanguage       : List (ProgrammingLanguage)
    , tools                     : List String
    , levelOfDifficulty         : Difficulty
    , description               : String
    , materialType              : String
    , tags                      : List String
    , tagsOpenArchaeo           : List String
    , link                      : String
    , citation                  : String
    }

type alias ProgrammingLanguage = { name : String }

type Difficulty =
      Beginner
    | Intermediate
    | Advanced

difficultyFromString : String -> Result String Difficulty
difficultyFromString s = case s of
    "beginner"      -> Ok Beginner
    "intermediate"  -> Ok Intermediate
    "advanced"      -> Ok Advanced
    _               -> Err "invalid diffculty string"

difficultyToString : Difficulty -> String
difficultyToString d = case d of
    Beginner        -> "beginner"
    Intermediate    -> "intermediate"
    Advanced        -> "advanced"

makeDummyResource : Float -> Float -> TeachingResource
makeDummyResource x y = {
      id = ""
    , partner = []
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
        decodeProgrammingLanguageList = decodeStringList |> Decode.map (List.map ProgrammingLanguage)
        decodeDifficulty = Decode.string |>
                           Decode.andThen (\value -> Decode.fromResult (difficultyFromString value))
    in Decode.into TeachingResource
            |> Decode.pipeline (Decode.field "ID" Decode.string)
            |> Decode.pipeline (Decode.field "Partner" decodeStringList)
            |> Decode.pipeline (Decode.field "X_map" Decode.float)
            |> Decode.pipeline (Decode.field "Y_map" Decode.float)
            |> Decode.pipeline (Decode.field "Name" Decode.string)
            |> Decode.pipeline (Decode.field "Author" decodeStringList)
            |> Decode.pipeline (Decode.field "Year" Decode.string)
            |> Decode.pipeline (Decode.field "Topic" Decode.string)
            |> Decode.pipeline (Decode.field "Language" Decode.string)
            |> Decode.pipeline (Decode.field "Programming_language" decodeProgrammingLanguageList)
            |> Decode.pipeline (Decode.field "Tools" decodeStringList)
            |> Decode.pipeline (Decode.field "Level_of_difficulty" decodeDifficulty)
            |> Decode.pipeline (Decode.field "Description" Decode.string)
            |> Decode.pipeline (Decode.field "Material_type" Decode.string)
            |> Decode.pipeline (Decode.field "Tags" decodeStringList)
            |> Decode.pipeline (Decode.field "Tags_openarchaeo" decodeStringList)
            |> Decode.pipeline (Decode.field "Link" Decode.string)
            |> Decode.pipeline (Decode.field "Citation" Decode.string)

teachingResources : List TeachingResource
teachingResources =
    case Decode.decodeCustom {fieldSeparator = '\t'} Decode.FieldNamesFromFirstRow decodeTeachingResource teachingMaterialString of
        Err x -> -- for debugging of .tsv file and parsing
                 --let _ = Debug.log "Parsing error" (Decode.errorToString x)
                 --in []
                 []
        Ok x -> x

-- INIT

init : Int -> List TeachingResource -> ( Model, Cmd Msg )
init wW elements =
    let model = { windowWidth = wW
                -- table and data
                , elements = teachingResources
                , tableState = Table.initialSort "ID"
                , nameQuery = ""
                , programmingLanguageQuery = ""
                , tagsQuery = ""
                -- map
                , center = { x = 100, y = 50 }
                , dragging = None
                , percentage = 100
                , hovering = []
                , clickedElement = Nothing
                -- modal
                , modalVisibility = Modal.hidden
                , selectedElement = Nothing
                -- welcome
                , welcomeVisibility = Modal.shown
                }
    in ( model, Cmd.none )

-- UPDATE

type Msg =
      SetWindowWidth Int
    -- table and data
    | SetNameQuery String
    | SetProgrammingLanguageQuery String
    | SetTagsQuery String
    | SetTableState Table.State
    | ClearFilter
    -- map
    | OnMouseClick (List (CI.One TeachingResource CI.Dot))
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
    -- welcome
    | CloseWelcome

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetWindowWidth w ->
            ({ model | windowWidth = w }, Cmd.none)
        -- map
        OnMouseClick hovering ->
            case (List.head (filterHoveringToRealEntries hovering)) of
                Nothing -> ({ model | clickedElement = Nothing }, Cmd.none)
                Just x -> ({ model | clickedElement = Just <| CI.getData x }, Cmd.none)
        OnMouseDown offset ->
            ({ model | dragging = CouldStillBeClick offset }, Cmd.none)
        OnMouseMove offset hovering ->
            case model.dragging of
                CouldStillBeClick prevOffset ->
                    if prevOffset == offset then
                        ({ model | hovering = hovering }, Cmd.none)
                    else
                        ({ model | center = updateCenter model.center prevOffset offset
                         , dragging = ForSureDragging offset
                         , hovering = hovering
                         }, Cmd.none)
                ForSureDragging prevOffset ->
                      ({ model | center = updateCenter model.center prevOffset offset
                      , dragging = ForSureDragging offset
                      , hovering = hovering
                      }, Cmd.none)
                None ->
                    ({ model | hovering = hovering }, Cmd.none)
        OnMouseUp offset coord ->
            case model.dragging of
                CouldStillBeClick prevOffset ->
                    ({ model | dragging = None }, Cmd.none)
                ForSureDragging prevOffset ->
                    ({ model | center = updateCenter model.center prevOffset offset
                     , dragging = None
                     }, Cmd.none)
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
        -- table and data
        SetNameQuery newQuery ->
            ({ model | nameQuery = newQuery }, Cmd.none)
        SetProgrammingLanguageQuery newQuery ->
            ({ model | programmingLanguageQuery = newQuery }, Cmd.none)
        SetTagsQuery newQuery ->
            ({ model | tagsQuery = newQuery }, Cmd.none)
        SetTableState newState ->
            ({ model | tableState = newState }, Cmd.none)
        ClearFilter ->
            ({ model |
               nameQuery = ""
             , programmingLanguageQuery = ""
             , tagsQuery = ""
             , clickedElement = Nothing
             }, Cmd.none)
        -- modal
        CloseModal ->
            ({ model | modalVisibility = Modal.hidden } , Cmd.none)
        ShowModal element ->
            ({ model | modalVisibility = Modal.shown, selectedElement = element } , Cmd.none)
        -- welcome
        CloseWelcome ->
            ({ model | welcomeVisibility = Modal.hidden } , Cmd.none)

updateCenter : CS.Point -> CS.Point -> CS.Point -> CS.Point
updateCenter center prevOffset offset = {
    x = center.x + (prevOffset.x - offset.x)
  , y = center.y + (prevOffset.y - offset.y)
  }

-- VIEW

view : Bool -> Model -> Html Msg
view devel 
    {   windowWidth,
        elements,
        tableState,
        nameQuery,
        programmingLanguageQuery,
        tagsQuery,
        center,
        dragging,
        percentage,
        hovering,
        clickedElement,
        modalVisibility,
        selectedElement,
        welcomeVisibility
    } =
    let
        -- general helper functions and settings
        breakWindowWidth : Int
        breakWindowWidth = 500 --px
        findElementByCoordinates x y =
            List.head <| List.filter (\e -> e.x == x && e.y == y) elements
        findElementByID i =
            List.head <| List.filter (\e -> e.id == i) elements

        -- map
        mapPlot = 
            C.chart
                -- general settings
                [ CA.height 300
                , CA.width 600
                , CA.range [ CA.zoom percentage, CA.centerAt center.x ]
                , CA.domain [ CA.zoom percentage, CA.centerAt center.y ]
                , CE.onClick OnMouseClick (CE.getWithin 20 CI.dots)
                , CE.onMouseDown OnMouseDown CE.getOffset
                , CE.on "mousemove" (CE.map2 OnMouseMove CE.getOffset (CE.getWithin 20 CI.dots))
                , CE.on "mouseup" (CE.map2 OnMouseUp CE.getOffset CE.getCoords)
                , CE.onMouseLeave OnMouseLeave
                , CA.htmlAttrs
                    [ HA.style "user-select" "none"
                    , HA.style "cursor" <|
                        case (filterHoveringToRealEntries hovering) of
                            [] -> case dragging of
                                      CouldStillBeClick _ -> "grabbing"
                                      ForSureDragging _ -> "grabbing"
                                      None -> "grab"
                            _ -> "pointer"
                    ]
                ]
                [
                -- comment to hide coordinate grid
                  C.xLabels [ CA.withGrid, CA.amount 10, CA.ints, CA.fontSize 9 ]
                , C.yLabels [ CA.withGrid, CA.amount 10, CA.ints, CA.fontSize 9 ]
                , C.xTicks [ CA.withGrid, CA.amount 10, CA.ints ]
                , C.yTicks [ CA.withGrid, CA.amount 10, CA.ints ]
                -- zoom buttons in top right corner
                , C.htmlAt .max .max -5 -5
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
                -- background map
                , C.svgAt (\_ -> 0) (\_ -> 100) 0 0 [ comparchmap [
                        attribute "width" (String.fromFloat (600 * (percentage / 100)))
                      , attribute "height" (String.fromFloat (300 * (percentage / 100)))
                      , attribute "viewBox" ("0 0 2000 1000")
                      --, attribute "opacity" "0.3" -- make transparent to set coordinates more easily
                    ] ]
                -- invisible dummy data points at the four corners to make the plot scale correctly
                , C.series .x [
                    C.scatter .y 
                        [ CA.opacity 0] ]
                            (List.map2 makeDummyResource [0, 0, 200, 200] [0, 100, 100, 0])
                -- additional elements for each point
                , C.each (filterHoveringToRealEntries hovering) <| \p item -> 
                    let curX = CI.getX item
                        curY = CI.getY item
                        curElem = findElementByCoordinates curX curY
                    in [
                        -- tooltip
                        C.tooltip item [
                            CA.offset 0
                          , CA.background "#fcf9e9"
                            ] [
                            HA.style "opacity" "0.8"
                            ] [
                              case curElem of
                                Nothing -> text ""
                                Just x -> text (x.id ++ ": " ++ x.name)
                            ]
                        -- lines to partner nodes
                        , C.withPlane <| \_ ->
                            case curElem of
                                Nothing -> []
                                Just x -> addLinesCurElement x
                    ]
                -- actual data points
                , C.series .x [
                    C.scatter .y 
                        [ CA.size 3
                        , CA.diamond
                        , CA.highlight 0.6
                        , CA.highlightWidth 2
                        , CA.highlightColor "white"
                        ] |>
                        C.named "Teaching resource" |>
                        C.amongst hovering (\_ -> [ CA.size 15 ]) |>
                        -- color by difficulty level
                        C.variation (\i d -> [
                            CA.color (case d.levelOfDifficulty of
                                Beginner -> CA.green
                                Intermediate -> CA.yellow
                                Advanced -> CA.red)
                        ])
                        ] elements -- actual input data
                ]

        -- plot helper functions
        addLinesCurElement : TeachingResource -> List (C.Element data msg)
        addLinesCurElement elem =
            let curPartners = values <| map findElementByID elem.partner
            in case curPartners of
                [] -> []
                xs -> (map (makeOneLine elem) xs) ++ List.concatMap addLinesCurElement xs

        makeOneLine : TeachingResource -> TeachingResource -> C.Element data msg
        makeOneLine elem partner =
            C.line [ CA.x1 elem.x
                   , CA.x2 partner.x
                   , CA.y1 elem.y
                   , CA.y2 partner.y
                   , CA.dashed [ 10, 5 ]
                   , CA.width 2
                   , CA.color "white"
                   ]

        -- search/filter logic
        lowerNameQuery = String.toLower nameQuery
        lowerProgrammingQuery = String.toLower programmingLanguageQuery
        lowerTagsQuery = String.toLower tagsQuery
        acceptableResources = 
            case clickedElement of
                Nothing -> List.filter (
                    (\x ->
                        let 
                            matchName = String.contains lowerNameQuery <| String.toLower <| (x.name ++ String.join "" x.author)
                            matchProg = String.contains lowerProgrammingQuery <| String.toLower <| String.join "" <| List.map (\l -> l.name) <| x.programmingLanguage
                            matchTag = String.contains lowerTagsQuery <| String.toLower <| String.join "" <| x.tags
                        in matchName && matchProg && matchTag
                        )
                    ) elements
                Just x -> [x]

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

        badgeStyle = [
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
                      , Button.attrs [ href s, style "margin-bottom" "-5px", style "width" "40px" ]
                      ] [ Icon.view Icon.link ]
                    , Button.button [ 
                        Button.small, Button.block, Button.outlineSecondary
                      , Button.attrs [ HE.onClick <| ShowModal (findElementByID (getID data)), style "margin-bottom" "10px", style "width" "40px" ]
                      ] [ Icon.view Icon.infoCircle ] 
                    ]
                ]) (getLink data)
            , sorter = Table.unsortable
            }

        tableConfig : Table.Config TeachingResource Msg
        tableConfig =
            Table.customConfig {
                  toId = .id
                , toMsg = SetTableState
                , columns =
                    if windowWidth < breakWindowWidth then [ 
                      idColumn "ID" .id
                    , resourceColumn "Material" .year .name .author
                    , linkAndModalColumn "" .link .id
                    ] else [ 
                      idColumn "ID" .id
                    , resourceColumn "Material" .year .name .author
                    , stringListColumn "Language" (\data -> List.map (\l -> l.name) data.programmingLanguage) viewProgrammingLanguage
                    , stringListColumn "Tags" .tags viewTags
                    , linkAndModalColumn "" .link .id
                    ]
                , customizations = { defaultCustomizations | tableAttrs = [ style "width" "100%" ] }
                }

        -- modal
        oneRow name value =
            Grid.row [ ] [
                  Grid.col [ Col.sm3 ]
                    [ span [ style "font-weight" "bold" ] [ text name ] ]
                , Grid.col [ Col.sm9 ] [ text value ]
            ]

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
                                , oneRow "Prog. language: "   <| String.join ", " <| List.map (\l -> l.name) x.programmingLanguage
                                , oneRow "Tools: "            <| String.join ", " x.tools
                                , oneRow "Level: "            <| difficultyToString x.levelOfDifficulty
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

        -- welcome
        detailsWelcome =
            Modal.config CloseWelcome
                |> Modal.large
                |> Modal.hideOnBackdropClick True
                |> Modal.h4 [] [ text "The Map of Computational Archaeology" ]
                |> Modal.body [] [
                    p [] [ text <| "Computational Archaeology is a wondrous field. "
                           ++ "To make it a bit easier to explore and navigate, this webapp "
                           ++ "presents an imaginary map with a collection of teaching material "
                           ++ "introducing the different domains of computer applications in archaeology."
                         ],
                    p [] [ text <| "The interactive map on the top shows the various "
                           ++ "subfields as landmasses of an imaginary world. "
                           ++ "Each dot represents a teaching resource (e.g. a blogpost, "
                           ++ "a video-tutorial or a textbook). The dot color indicates, "
                           ++ "how advanced and challenging a given unit is:"
                         ],
                    p [] [ span [ style "color" "#71C614" ] [ text "Suitable for beginners" ],
                           text " -> ",
                           span [ style "color" "#FFCA00" ] [ text "Intermediate" ],
                           text " -> ",
                           span [ style "color" "#EF3159" ] [ text "Advanced" ]
                         ],
                    p [] [ text <| "You can hover or click on the dots to get more information. "
                           ++ "Click ",
                           span [ style "color" "#EF3159" ] [ text "Clear filters" ],
                           text <| " to reset the list below the map."
                         ]
                ]
                |> Modal.footer [] [ span [ style "font-style" "italic" ] [
                                text "Made by the "
                              , a [ href "https://sslarch.github.io" ] [ text "SIG for Scripting languages in Archaeology" ]
                              , text " - see the code on "
                              , a [ href "https://github.com/sslarch/MapofComputationalArchaeology" ] [ text "Github" ]
                              ] ]
                |> Modal.view welcomeVisibility

        -- layout helper functions
        query1 = input [ style "width" "100%", style "margin" "1px", placeholder "by Name and Authors", onInput SetNameQuery ] []
        query2 = input [ style "width" "100%", style "margin" "1px", placeholder "by Language", onInput SetProgrammingLanguageQuery ] []
        query3 = input [ style "width" "100%", style "margin" "1px", placeholder "by Tag", onInput SetTagsQuery ] []

    in
        -- main layout
        div [] [
            Grid.container [] 
                ((if devel then [CDN.stylesheet] else []) ++ [ -- stylesheet for production is loaded in index.html
                  Icon.css -- fontawesome
                , Grid.row [] [
                      Grid.col [ ] [
                          div [ 
                              style "overflow" "hidden"
                            , style "margin" "auto"
                            , style "height" "100%"
                            , style "width" "100%"
                            ] [ mapPlot ] 
                        ]
                    ]
                , Grid.row [] [
                    Grid.col [ ] [
                          br [] []
                        , div [] [
                                span [ style "font-size" "30px" ] [ text "Computational archaeology teaching material list" ]
                              , span [ style "display" "inline-block", style "width" "20px" ] []
                              , text " a project by the "
                              , a [ href "https://sslarch.github.io" ] [ text "SIG SSLA" ]
                            ]
                        , Alert.simpleDark [] [
                            Grid.container [] [ 
                                Grid.row [ Row.centerMd ] (
                                    [
                                        Grid.col [] [
                                            div [ style "display" "inline-block", style "width" "100%" ] [
                                                  Icon.view Icon.filter
                                                , text <| " Filter the list (" ++ 
                                                    (String.fromInt <| List.length acceptableResources) ++
                                                    "/" ++
                                                    (String.fromInt <| List.length elements) ++ 
                                                    ") "
                                                , Button.button [ 
                                                    Button.attrs [ style "float" "right" ]
                                                  , Button.small, Button.outlineDanger
                                                  , Button.attrs [ HE.onClick ClearFilter ]
                                                  ] [ Icon.view Icon.filterCircleXmark, text " Clear filters" ]
                                            ]
                                        ]
                                    , Grid.colBreak []
                                    ] ++ if windowWidth < breakWindowWidth then [
                                        Grid.col [] [ query1, query2, query3 ]
                                    ] else [
                                      Grid.col [] [ query1 ]
                                    , Grid.col [] [ query2 ]
                                    , Grid.col [] [ query3 ]
                                    ]
                                )
                            ]
                        ]
                        , Table.view tableConfig tableState acceptableResources
                        ]
                    ]
                ])
            , detailsModal, detailsWelcome
            ]

filterHoveringToRealEntries : List (CI.One TeachingResource CI.Dot) -> List (CI.One TeachingResource CI.Dot)
filterHoveringToRealEntries x = (List.filter (\y -> (CI.getData y).id /= "") x)

-- SUBSCRIPTIONS

subscriptions : model -> Sub Msg
subscriptions _ =
    E.onResize (\w h -> SetWindowWidth w)
