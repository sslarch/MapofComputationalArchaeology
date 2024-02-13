module Main exposing (..)

import MapOfComputionalArchaeology exposing (comparchmap)
import TeachingMaterial exposing (
    TeachingResource, Difficulty (..),
    makeDummyResource, difficultyToString, parseTeachingResources)

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
import Browser.Navigation as Navigation
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Chart.Svg as CS
import FontAwesome as Icon exposing (Icon)
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Html as H exposing (Html, a, br, button, div, h1, input, p, span, text)
import Html.Attributes as HA exposing (href, placeholder, style)
import Html.Events as HE exposing (onInput)
import Http as Http
import List exposing (map, concat, sort, any, member)
import Maybe.Extra exposing (values)
import Select as Select
import Simple.Fuzzy as SF
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
    , multiQueryContent         : List String
    , multiQuery                : QueryModel
    -- map
    , center                    : CS.Point
    , dragging                  : Dragging
    , percentage                : Float
    , hovering                  : Maybe CE.Point
    , closestPoint              : List (CI.One TeachingResource CI.Dot)
    -- modal
    , modalVisibility           : Modal.Visibility
    , selectedElement           : Maybe TeachingResource
    -- welcome
    , welcomeVisibility         : Modal.Visibility
    -- error
    , errorVisibility           : Modal.Visibility
    , errorMessage              : String
    }

type alias QueryModel =
    { id : String
    , available : List String
    , itemToLabel : String -> String
    , selected : List String
    , selectState : Select.State
    , selectConfig : Select.Config (MultiQueryMsg String) String
    }

type MultiQueryMsg item
    = NoOp
    | OnSelect (Maybe item)
    | OnRemoveItem item
    | SelectMsg (Select.Msg item)

type Dragging =
    CouldStillBeClick CS.Point
  | ForSureDragging CS.Point
  | None

-- INIT

init : Int -> List TeachingResource -> ( Model, Cmd Msg )
init wW resources =
    let model = { windowWidth = wW
                -- table and data
                , elements = resources
                , tableState = Table.sortBy "ID" True
                , multiQueryContent = [ ]
                , multiQuery = {
                    id = "exampleMulti"
                  , available = (map .tags resources |> concat) ++ (map .programmingLanguage resources |> concat) |> sort
                  , itemToLabel = identity
                  , selected = [ ]
                  , selectState = Select.init ""
                  , selectConfig = Select.newConfig
                        { onSelect = OnSelect
                        , toLabel = identity
                        , filter = filter 0 identity
                        , toMsg = SelectMsg
                        }
                        |> Select.withMultiSelection True
                        |> Select.withCustomInput identity
                        |> Select.withOnRemoveItem OnRemoveItem
                        |> Select.withCutoff 12
                        |> Select.withNotFound "No matches"
                        |> Select.withPrompt "by title, authors, language or tags"
                }
                -- map
                , center = { x = 100, y = 50 }
                , dragging = None
                , percentage = 100
                , hovering = Nothing
                , closestPoint = []
                -- modal
                , modalVisibility = Modal.hidden
                , selectedElement = Nothing
                -- welcome
                , welcomeVisibility = Modal.shown
                -- error
                , errorVisibility = Modal.hidden
                , errorMessage    = ""
                }
    in ( model, Cmd.none )

filter : Int -> (a -> String) -> String -> List a -> Maybe (List a)
filter minChars toLabel query items =
    if String.length query < minChars then
        Nothing
    else
        items
            |> SF.filter toLabel query
            |> Just

-- UPDATE

type Msg =
      SetWindowWidth Int
    -- download data
    | SendHttpRequest
    | DataReceived (Result Http.Error String)
    -- table and data
    | SetMultiQuery (MultiQueryMsg String)
    | ButtonAddToQuery String
    | SetTableState Table.State
    | ClearFilter
    -- map
    | OnMouseClick (List (CI.One TeachingResource CI.Dot))
    | OnMouseDown CS.Point
    | OnMouseMove CS.Point CE.Point (List (CI.One TeachingResource CI.Dot))
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
    -- error
    | CloseError
    | ShowError String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetWindowWidth w ->
            ({ model | windowWidth = w }, Cmd.none)
        -- download data
        SendHttpRequest -> ( model, getTeachingResources )
        DataReceived res -> case res of
            Err e -> update (ShowError "Failed to download data from GitHub.") model
            Ok  x ->
                case parseTeachingResources x of
                    Err e -> update (ShowError ("Can't parse data. " ++ e)) model
                    Ok  y -> ({ model | elements = y }, Cmd.none)
        -- map
        OnMouseClick closestPoint ->
            case (List.head (filterClosestPointToRealEntries closestPoint)) of
                Nothing -> (model, Cmd.none)
                Just x -> update (ShowModal (Just <| CI.getData x)) model
        OnMouseDown offset ->
            ({ model | dragging = CouldStillBeClick offset }, Cmd.none)
        OnMouseMove offset hovering closestPoint ->
            case model.dragging of
                CouldStillBeClick prevOffset ->
                    if prevOffset == offset then
                        ({ model | closestPoint = closestPoint
                         , hovering = Just hovering }, Cmd.none)
                    else
                        ({ model | center = updateCenter model.center prevOffset offset
                         , dragging = ForSureDragging offset
                         , closestPoint = closestPoint
                         , hovering = Just hovering
                         }, Cmd.none)
                ForSureDragging prevOffset ->
                      ({ model | center = updateCenter model.center prevOffset offset
                      , dragging = ForSureDragging offset
                      , closestPoint = closestPoint
                      , hovering = Just hovering
                      }, Cmd.none)
                None ->
                    ({ model | closestPoint = closestPoint
                     , hovering = Just hovering }, Cmd.none)
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
            ({ model | dragging = None, closestPoint = [], hovering = Nothing }, Cmd.none)
        OnZoomIn ->
            ({ model | percentage = model.percentage + 20 }, Cmd.none)
        OnZoomOut ->
            ({ model | percentage = max 1 (model.percentage - 20) }, Cmd.none)
        OnZoomReset ->
            ({ model | percentage = 100, center = { x = 100, y = 50 } }, Cmd.none)
        -- table and data
        SetMultiQuery sub ->
            let ( newMultiQuery, subCmd ) = updateMultiQuery sub model.multiQuery
            in  ({ model | multiQueryContent = newMultiQuery.selected, multiQuery = newMultiQuery }, Cmd.map SetMultiQuery subCmd)
        ButtonAddToQuery s ->
            let oldMultiQuery = model.multiQuery
                newMultiQuery = { oldMultiQuery | selected = oldMultiQuery.selected ++ [s] }
            in ({ model |
               multiQueryContent = newMultiQuery.selected
             , multiQuery = newMultiQuery
             }, Cmd.none)
        SetTableState newState ->
            ({ model | tableState = newState }, Cmd.none)
        ClearFilter ->
            let oldMultiQuery = model.multiQuery
                newMultiQuery = { oldMultiQuery | selected = [ ] }
            in ({ model |
               multiQueryContent = [ ]
             , multiQuery = newMultiQuery
             }, Cmd.none)
        -- modal
        CloseModal ->
            ({ model | modalVisibility = Modal.hidden } , Cmd.none)
        ShowModal element ->
            ({ model | modalVisibility = Modal.shown, selectedElement = element } , Cmd.none)
        -- welcome
        CloseWelcome ->
            ({ model | welcomeVisibility = Modal.hidden }, getTeachingResources)
        -- error
        CloseError ->
            ({ model | errorVisibility = Modal.hidden }, Navigation.reload)
        ShowError e ->
            ({ model | errorVisibility = Modal.shown, errorMessage = e }, Cmd.none)

getTeachingResources : Cmd Msg
getTeachingResources =
    Http.get {
          url = "https://raw.githubusercontent.com/sslarch/MapofComputationalArchaeology/main/data/teachingmaterial.tsv"
          --url = "https://raw.githubusercontent.com/poseidon-framework/community-archive/07879ea4828b8e6d3b39cfbbf5bd51f6133a971f/2019_Jeong_InnerEurasia/POSEIDON.yml"
        , expect = Http.expectString DataReceived
        }

updateMultiQuery : MultiQueryMsg String -> QueryModel -> ( QueryModel, Cmd (MultiQueryMsg String) )
updateMultiQuery msg model =
    case msg of
        OnSelect maybeColor ->
            let
                selected =
                    maybeColor
                        |> Maybe.map (List.singleton >> List.append model.selected)
                        |> Maybe.withDefault []
            in
            ( { model | selected = selected }, Cmd.none )
        OnRemoveItem colorToRemove ->
            let
                selected =
                    List.filter (\curColor -> curColor /= colorToRemove)
                        model.selected
            in
            ( { model | selected = selected }, Cmd.none )
        SelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update
                        model.selectConfig
                        subMsg
                        model.selectState
            in
            ( { model | selectState = updated }, cmd )
        NoOp ->
            ( model, Cmd.none )

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
        multiQueryContent,
        multiQuery,
        center,
        dragging,
        percentage,
        hovering,
        closestPoint,
        modalVisibility,
        selectedElement,
        welcomeVisibility,
        errorVisibility,
        errorMessage
    } =
    let
        -- general helper functions and settings
        breakWindowWidth : Int
        breakWindowWidth = 500 --px
        findElementByCoordinates x y =
            List.head <| List.filter (\e -> e.x == x && e.y == y) elements
        findElementByID i =
            List.head <| List.filter (\e -> e.id == i) acceptableResources

        -- search/filter logic
        acceptableResources = case multiQueryContent of
            [ ] -> elements
            _   -> List.filter resourceFilter elements

        resourceFilter : TeachingResource -> Bool
        resourceFilter x =
            let toLow = String.toLower
                matchName   = any (\v -> String.contains (toLow v) (toLow x.name)) multiQueryContent
                matchAuthor = any (\v -> String.contains (toLow v) (toLow <| String.join "" x.author)) multiQueryContent
                matchProg   = any (\v -> member v multiQueryContent) x.programmingLanguage
                matchTag    = any (\v -> member v multiQueryContent) x.tags
            in  matchName || matchAuthor || matchProg || matchTag

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
                , CE.on "mousemove" (CE.map3 OnMouseMove CE.getOffset CE.getCoords (CE.getWithin 20 CI.dots))
                , CE.on "mouseup" (CE.map2 OnMouseUp CE.getOffset CE.getCoords)
                , CE.onMouseLeave OnMouseLeave
                , CA.htmlAttrs
                    [ HA.style "user-select" "none"
                    , HA.style "cursor" <|
                        case (filterClosestPointToRealEntries closestPoint) of
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
                , C.each (filterClosestPointToRealEntries closestPoint) <| \p item -> 
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
                        -- lines to source nodes
                        , C.withPlane <| \_ ->
                            case curElem of
                                Nothing -> []
                                Just x -> addLinesCurElement x
                    ]
                -- actual data points
                , C.series .x [
                    C.scatter .y 
                        [ CA.size 2
                        , CA.diamond
                        , CA.highlight 0.6
                        , CA.highlightWidth 2
                        , CA.highlightColor "#292929"
                        ] |>
                        C.named "Teaching resource" |>
                        C.amongst closestPoint (\_ -> [ CA.size 12 ]) |>
                        -- color by difficulty level
                        C.variation (\i d -> [
                            CA.color (case d.levelOfDifficulty of
                                Beginner -> CA.green
                                Intermediate -> CA.yellow
                                Advanced -> CA.red)
                        ])
                        ] acceptableResources -- actual input data
                -- coord display below plot
                , case hovering of
                    Just coords ->
                      C.labelAt (CA.percent 96) (CA.percent 2) [CA.fontSize 7]
                        [ S.text ("x: " ++ String.fromInt (round coords.x))
                        , S.text (" y: " ++ String.fromInt (round coords.y))
                        ]
                    Nothing ->
                      C.none
                ]

        -- plot helper functions
        addLinesCurElement : TeachingResource -> List (C.Element data msg)
        addLinesCurElement elem =
            let curSources = values <| map findElementByID elem.source
            in case curSources of
                [] -> []
                xs -> (map (makeOneLine elem) xs) ++ List.concatMap addLinesCurElement xs

        makeOneLine : TeachingResource -> TeachingResource -> C.Element data msg
        makeOneLine elem source =
            C.line [ CA.x1 elem.x
                   , CA.x2 source.x
                   , CA.y1 elem.y
                   , CA.y2 source.y
                   , CA.dashed [ 10, 5 ]
                   , CA.width 2
                   , CA.color "white"
                   ]

        -- table
        idColumn : String -> (data -> String) -> Table.Column data Msg
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

        resourceColumn : String -> (data -> String) -> (data -> String) -> (data -> List String) -> Table.Column data Msg
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

        viewProgrammingLanguage : List String -> Table.HtmlDetails Msg
        viewProgrammingLanguage ss = Table.HtmlDetails [] (map (makeButton "#80b3ffff") ss)

        viewTags : List String -> Table.HtmlDetails Msg
        viewTags ss = Table.HtmlDetails [] (map (makeButton "#bfb891ff") ss)

        makeButton : String -> String -> H.Html Msg
        makeButton color s = case s of
            "" -> H.div [] []
            _  -> Button.button
                    [ Button.attrs [
                          HE.onClick (ButtonAddToQuery s)
                        , Spacing.ml1
                        , style "background-color" color
                        , style "color" "white"
                        , style "display" "inline-block"
                        , style "padding" "1px 4px"
                        , style "text-align" "center"
                        , style "border-radius" "5px"
                        , style "margin" "2px"
                        , style "font-size" "15px"
                        ]
                    , Button.outlineDark
                    , Button.small
                    ]
                    [ text s ]

        stringListColumn : String -> (data -> List String) -> (List String -> Table.HtmlDetails Msg) -> Table.Column data Msg
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
                    , stringListColumn "Language" .programmingLanguage viewProgrammingLanguage
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
                                , oneRow "Prog. language: "   <| String.join ", " x.programmingLanguage
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
                |> Modal.h4 [] [ text "The didactic map of computational archaeology" ]
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
                           text <| " to reset the list."
                         ]
                ]
                |> Modal.footer [] [ span [ style "font-style" "italic" ] [
                                text "Made by the "
                              , a [ href "https://sslarch.github.io" ] [ text "SIG for Scripting languages in Archaeology" ]
                              , text " - see the code on "
                              , a [ href "https://github.com/sslarch/MapofComputationalArchaeology" ] [ text "GitHub" ]
                              ] ]
                |> Modal.view welcomeVisibility

        -- error
        detailsError =
            Modal.config CloseError
                |> Modal.large
                |> Modal.hideOnBackdropClick False
                |> Modal.scrollableBody True
                |> Modal.h3 [] [ text "Error" ]
                |> Modal.body [] [
                    p [] [ H.pre [] [text errorMessage] ],
                    p [] [
                      text "Please report this error on "
                    , a [ href "https://github.com/sslarch/MapofComputationalArchaeology" ] [ text "GitHub" ]
                    , text "."
                    ]
                ]
                |> Modal.view errorVisibility

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
                                span [ style "font-size" "30px" ] [ text "The didactic map of computational archaeology" ]
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
                                    , Grid.col []
                                        [ H.map SetMultiQuery (
                                            div [] [
                                              Select.view multiQuery.selectConfig
                                                          multiQuery.selectState
                                                          multiQuery.available
                                                          multiQuery.selected
                                            ])

                                        ]
                                    ]
                                )
                            ]
                        ]
                        , Table.view tableConfig tableState acceptableResources
                        ]
                    ]
                ])
            , detailsModal, detailsWelcome, detailsError
            ]

filterClosestPointToRealEntries : List (CI.One TeachingResource CI.Dot) -> List (CI.One TeachingResource CI.Dot)
filterClosestPointToRealEntries x = (List.filter (\y -> (CI.getData y).id /= "") x)

-- SUBSCRIPTIONS

subscriptions : model -> Sub Msg
subscriptions _ =
    E.onResize (\w h -> SetWindowWidth w)
