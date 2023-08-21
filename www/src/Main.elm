module Main exposing (main)

import Debug
import Bitwise
import Browser
import Browser.Dom
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes
import Set exposing (Set)
import Task

import Colors.Alpha as A
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Encode
import Material.Icons as Filled
import Material.Icons.Types exposing (Coloring(..), Icon)
import TypedSvg exposing (circle, rect, svg)
import TypedSvg.Attributes exposing
    ( x, y, height, width, cx, cy, r
    , stroke, strokeOpacity, fill, fillOpacity, strokeWidth
    , viewBox
    )
import TypedSvg.Core exposing (Svg, Attribute)
import TypedSvg.Events
import TypedSvg.Types exposing (Opacity(..), Paint(..), px)

import Fragment exposing (Fragment)
import Rule exposing
    ( Rule, RuleType(..)
    , Error(..)
    , Case,  Combination, Progeny, Mobility
    , newRule
    , getSpan, getType, getCases
    , updateMobility, updateProgeny, encodeRule
    , getCombination, getProgeny, getMobility, getCases
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- MODEL


type Cell
    = Live
    | Dead


type alias Model =
    { settings : Settings
    , rule : Rule Cell
    , state : State
    , canvas : Canvas
    }


type SimulationState
    = Paused
    | Running


type alias State =
    { simulation : SimulationState
    }


type alias Highlighted =
    { ruleCase : Case Cell
    , index : Int
    }

type Editing
    = Progeny  (Maybe Highlighted)
    | Mobility (Maybe Highlighted)


type alias Settings =
    { speed : Int
    , cellSize : Int
    , editing : Editing
    }


type alias Canvas =
    { width : Float
    , height : Float
    }


type Error
    = BadRuleUpdate String


init : () -> (Model, Cmd Msg)
init _ =
    ( { settings =
        { speed = 1
        , cellSize = 3
        , editing = Progeny Nothing
        }
      , rule = newRule Simple 1 [Dead, Live] Dead
      , state =
          { simulation = Paused
          }
      , canvas =
        { width = 0
        , height = 0
        }
      }
    , Task.perform ResizeCanvas Browser.Dom.getViewport
    )


-- UPDATE


type SettingsMsg
    = ResizeCells Int
    | UpdateEditing Editing


updateSettings : SettingsMsg -> Settings -> Settings
updateSettings settingsMsg settings =
    case settingsMsg of
        ResizeCells newCellSize ->
            settings

        UpdateEditing newEditing ->
            { settings | editing = newEditing }


type StateMsg
    = UpdateSimulation SimulationState


updateState : StateMsg -> State -> State
updateState stateMsg state =
    case stateMsg of
        UpdateSimulation newSimulationState ->
            { state | simulation = newSimulationState }


type RuleMsg a
    = UpdateMobility (Case Cell) (a -> Result Error Mobility) a
    | UpdateProgeny  (Case Cell) (a -> Result Error (Progeny Cell)) a
    | UpdateType RuleType


updateRule : RuleMsg a -> Rule Cell -> Result Rule.Error (Rule Cell)
updateRule msg rule =
    case msg of
        UpdateMobility ruleCase mobilityModifier index ->
            let
                newMobilityResult : Result Error Mobility
                newMobilityResult = mobilityModifier index
            in
            case newMobilityResult of
                Ok newMobility ->
                    let
                        test = Debug.log "mobility update error" newMobility
                    in
                    updateMobility (getCombination ruleCase) newMobility rule

                Err error ->
                    let
                        test = Debug.log "mobility update error" error
                    in
                    Ok rule

        UpdateProgeny ruleCase progenyModifier index ->
            let
                newProgenyResult : Result Error (Progeny Cell)
                newProgenyResult = progenyModifier index
            in
            case newProgenyResult of
                Ok newProgeny ->
                    updateProgeny (getCombination ruleCase) newProgeny rule

                Err error ->
                    Ok rule

        UpdateType ruleType ->
            case ruleType of
                Simple ->
                    Ok (newRule Simple 1 [Dead, Live] Dead)

                Extended ->
                    Ok (newRule Extended 1 [Dead, Live] Dead)

                Generalized ->
                    Ok (newRule Generalized 1 [Dead, Live] Dead)


type Msg
    = ResizeCanvas Browser.Dom.Viewport
    | UpdateSettings SettingsMsg
    | UpdateRule (RuleMsg Int)
    | UpdateState StateMsg
    | HighlightError Error


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ResizeCanvas viewport ->
            ( { model
                | canvas = Canvas viewport.scene.width viewport.scene.height
              }
            , Cmd.none
            )

        UpdateRule ruleMsg ->
            let
                newRuleResult : Result Rule.Error (Rule Cell)
                newRuleResult =
                    updateRule ruleMsg model.rule
            in
            case newRuleResult of
                Err error ->
                    ( model
                    , Cmd.none
                    )

                Ok newRule ->
                    ( { model
                        | rule = newRule
                      }
                    , Cmd.none
                    )

        UpdateState stateMsg ->
            ( { model
                | state = updateState stateMsg model.state
              }
            , Cmd.none
            )

        UpdateSettings settingsMsg ->
            ( { model
                | settings = updateSettings settingsMsg model.settings
              }
            , Cmd.none
            )

        HighlightError error ->
            let
                test = Debug.log "error" error
            in
            ( model
            , Cmd.none
            )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW


view : Model -> Html Msg
view model =
    E.layout [] (viewPage model)


viewPage : Model -> Element Msg
viewPage model =
    E.column
        [ E.centerX
        , E.width E.fill
        , E.height E.fill
        ]
        [ viewControls model
        , viewCases model.settings.editing model.rule
        , viewTape model
        ]


viewControls : Model -> Element Msg
viewControls model =
    E.row
        [ E.centerX
        , E.padding 10
        , E.spacingXY 20 0
        ]
        [ viewPlayControls model.state
        , viewRuleControls (getType model.rule)
        , viewCellSizeControls model.settings.cellSize
        , viewEditingControls model.settings.editing
        ]


viewRuleControls : RuleType -> Element Msg
viewRuleControls ruleType =
    Input.radioRow
        []
        { onChange = UpdateRule << UpdateType
        , selected = Just ruleType
        , label = viewControlLabel "Rule Type"
        , options =
            [ Simple, Extended, Generalized ] |> List.map viewRuleTypeOption
        }


viewRuleTypeOption : RuleType -> Input.Option RuleType msg
viewRuleTypeOption ruleType =
    let
        description : String
        description =
            case ruleType of
                Simple ->
                    "Simple"

                Extended ->
                    "Extended"

                Generalized ->
                    "Generalized"
    in
    Input.option ruleType (E.text description)


viewPlayControls : State -> Element Msg
viewPlayControls state =
    let
        s : { action : Msg, icon : Icon msg }
        s =
            case state.simulation of
                Paused ->
                    { action = UpdateState (UpdateSimulation Running)
                    , icon = Filled.play_arrow
                    }

                Running ->
                    { action = UpdateState (UpdateSimulation Paused)
                    , icon = Filled.pause
                    }
    in
    Input.button []
        { onPress = Just s.action
        , label = E.html <| s.icon 32 Inherit
        }


viewCellSizeControls : Int -> Element Msg
viewCellSizeControls cellSize =
    E.el
        [ E.width (E.px 300)
        , E.alignTop
        ] <|
            Input.slider
                [ E.height (E.px 30)
                , E.behindContent <|
                    -- Slider track
                    E.el
                        [ E.width E.fill
                        , E.height <| E.px 3
                        , E.centerY
                        , Background.color (A.lightcoral 1)
                        , Border.rounded 2
                        ]
                        E.none
                ]
                { onChange = round >> (UpdateSettings << ResizeCells)
                , label = viewControlLabel ("Cell size: " ++ (String.fromInt cellSize)) 
                , min = 1
                , max = 15
                , value = toFloat cellSize
                , step = Just 1
                , thumb = Input.defaultThumb
                }


viewEditingControls : Editing -> Element Msg
viewEditingControls editing =
    let
        editingMode : String
        editingMode =
            case editing of
                Progeny _  -> "progeny"
                Mobility _ -> "mobility"
    in
    Input.radioRow
        [ Border.rounded 5
        ]
        { onChange = UpdateSettings << UpdateEditing
        , selected =
            case editing of
                Progeny _ ->
                    Just <| Progeny Nothing

                Mobility _ ->
                    Just <| Mobility Nothing
        , label = viewControlLabel ("Editing: " ++ editingMode)
        , options =
            [ Input.optionWith (Progeny Nothing) <| radioButton First <| Filled.crop_square 20 Inherit
            , Input.optionWith (Mobility Nothing) <| radioButton Last  <| Filled.circle 20 Inherit
            ]
        }


type RadioButtonPosition
    = First
    | Middle
    | Last


radioButton : RadioButtonPosition -> Svg Msg -> Input.OptionState -> Element Msg
radioButton position icon state =
    let
        borders =
            case position of
                First ->
                    { left = 3, right = 3, top = 3, bottom = 3 }

                Middle ->
                    { left = 0, right = 3, top = 3, bottom = 3 }

                Last ->
                    { left = 0, right = 3, top = 3, bottom = 3 }

        corners =
            case position of
                First ->
                    { topLeft = 6, bottomLeft = 6, topRight = 0, bottomRight = 0 }

                Middle ->
                    { topLeft = 0, bottomLeft = 0, topRight = 0, bottomRight = 0 }

                Last ->
                    { topLeft = 0, bottomLeft = 0, topRight = 6, bottomRight = 6 }
    in
    E.el
        [ E.paddingEach { left = 10, right = 10, top = 5, bottom = 5 }
        , Border.roundEach corners
        , Border.widthEach borders
        , Border.color<| A.lightcoral 1
        , Background.color <|
            if state == Input.Selected then
                A.lightcoral 1

            else
                A.white 1
        ] <|
            E.el [ E.centerX, E.centerY ] <|
                E.html <| icon


viewControlLabel : String -> Input.Label msg
viewControlLabel content =
    Input.labelAbove
        [ E.centerY
        , Font.size 12
        ] <|
            E.row
                [ E.spaceEvenly
                , E.width E.fill
                ]
                [ E.text content
                , E.html <| Filled.question_mark 12 Inherit
                ]


viewCases : Editing -> Rule Cell -> Element Msg
viewCases editing rule =
    let
        span : Int
        span =
            getSpan rule

        ruleType: RuleType
        ruleType =
            getType rule
    in
    E.row
        [ E.width E.fill
        , E.centerX
        , E.centerY
        , E.spacingXY 5 0
        , E.padding 10
        , E.scrollbarX
        , Background.color (A.lightcoral 1)
        ]
        ( getCases rule
            |> List.sortBy caseToInt
            |> List.map (drawCase editing span ruleType)
            |> List.map (E.el [ E.centerX ])
            |> List.reverse
        )


getProgenyEvents : Case Cell -> Int -> CellEvents Msg
getProgenyEvents ruleCase progenyIndex =
    let
        clickMsg : Int -> Msg
        clickMsg index =
            UpdateRule <| UpdateProgeny ruleCase (toggleProgeny <| getProgeny ruleCase) index

        hoverMsg : Int -> Msg
        hoverMsg index =
            UpdateSettings <| UpdateEditing <| Progeny <| Just (Highlighted ruleCase index)

        leaveMsg : Msg
        leaveMsg =
            UpdateSettings <| UpdateEditing <| Progeny Nothing
    in
    { click = clickMsg progenyIndex
    , hover = hoverMsg progenyIndex
    , leave = leaveMsg
    }


getMobilityEvents : RuleType -> Case Cell -> Int -> CellEvents Msg
getMobilityEvents ruleType ruleCase mobilityIndex =
    let
        clickMsg : Int -> Msg
        clickMsg index =
            case ruleType of
                Simple ->
                    UpdateRule <| UpdateMobility ruleCase replaceMobility index

                Extended ->
                    UpdateRule <| UpdateMobility ruleCase replaceMobility index

                Generalized ->
                    UpdateRule <| UpdateMobility ruleCase (toggleMobility <| getMobility ruleCase) index

        hoverMsg : Int -> Msg
        hoverMsg index =
            UpdateSettings <| UpdateEditing <| Mobility <| Just (Highlighted ruleCase index)

        leaveMsg : Msg
        leaveMsg =
            UpdateSettings <| UpdateEditing <| Mobility Nothing
    in
    { click = clickMsg mobilityIndex
    , hover = hoverMsg mobilityIndex
    , leave = leaveMsg
    }


highlightProgeny : Case Cell -> Maybe Highlighted -> Int -> Bool
highlightProgeny ruleCase highlightedOption index =
    case highlightedOption of
        Just highlighted ->
            if highlighted.ruleCase == ruleCase then
                index == highlighted.index
            else
                False

        Nothing -> False


highlightMobility : RuleType -> Case Cell -> Maybe Highlighted -> Int -> Bool
highlightMobility ruleType ruleCase highlightedOption index =
    case highlightedOption of
        Just highlighted ->
            if highlighted.ruleCase == ruleCase then
                case ruleType of
                    Simple ->
                        index == highlighted.index && not (Set.member index <| getMobility ruleCase) && index /= 0

                    Extended ->
                        index == highlighted.index && not (Set.member index <| getMobility ruleCase)

                    Generalized ->
                        index == highlighted.index

            else
                False

        Nothing -> False


drawCase : Editing -> Fragment.Span -> RuleType -> Case Cell -> Element Msg
drawCase editing span ruleType ruleCase =
    let
        -- SVG viewbox
        drawDimensions : Offset
        drawDimensions =
            addOffset (Offset 2 2) (getCellOffset (2 * span + 1) 2)

        progenyHighlighter : Maybe (Int -> Bool)
        progenyHighlighter =
            case editing of
                Progeny highlightedOption -> Just <| highlightProgeny ruleCase highlightedOption 
                _ -> Nothing

        mobilityHighlighter : Maybe (Int -> Bool)
        mobilityHighlighter =
            case editing of
                Mobility highlightedOption -> Just <| highlightMobility ruleType ruleCase highlightedOption
                _ -> Nothing

        eventConfiguration : Int -> CellEvents Msg
        eventConfiguration =
            case editing of
                Progeny _ ->
                    getProgenyEvents ruleCase

                Mobility _ ->
                    getMobilityEvents ruleType ruleCase
    in
    E.html <|
        TypedSvg.svg
            [ width  <| px <| drawDimensions.y
            , height <| px <| drawDimensions.x
            , viewBox -1 -1 drawDimensions.y drawDimensions.x
            ]
            [ drawCombination (getCellOffset 0 0) <|
                getCombination ruleCase
            , drawProgeny (getCellOffset 1 span) progenyHighlighter <|
                getProgeny ruleCase
            , drawStates span mobilityHighlighter <|
                getMobility ruleCase
            , drawEventCells eventConfiguration span ruleCase
            ]


drawCombination : Offset -> Combination Cell -> Svg Msg
drawCombination offset combination =
    combination
        |> Fragment.toIndexedList
        |> List.indexedMap
            ( \i (index, cell) ->
                drawCell (addOffset offset (getCellOffset 0 i)) cell False
            )
        |> TypedSvg.g []


drawProgeny : Offset -> Maybe (Int -> Bool) -> Progeny Cell -> Svg Msg
drawProgeny offset highlighterOption progeny =
    progeny
        |> Fragment.toIndexedList
        |> List.map
            ( \(index, cell) ->
                drawCell (addOffset offset (getCellOffset 0 index)) cell <|
                    case highlighterOption of
                        Nothing -> False
                        Just highlighter -> highlighter index
            )
        |> TypedSvg.g []


type alias Offset =
    { x : Float
    , y : Float
    }


getCellOffset : Int -> Int -> Offset
getCellOffset row col =
    let
        indexToDistance: Int -> Float
        indexToDistance index =
            toFloat (cellDrawSize * index + cellDrawSpacing * (index + 1))
    in
    Offset (indexToDistance col) (indexToDistance row)


addOffset : Offset -> Offset -> Offset
addOffset offset1 offset2 =
    Offset (offset1.x + offset2.x) (offset1.y + offset2.y)


cellDrawSize : Int
cellDrawSize = 15


cellDrawSpacing : Int
cellDrawSpacing = 0


getCellColor : Cell -> Color
getCellColor cell =
    case cell of
        Dead ->
            Color.white

        Live ->
            Color.rgb255 39 158 255


getHighlightedCellColor : Cell -> Color
getHighlightedCellColor cell =
    case cell of
        Dead ->
            Color.rgb255 64 248 255

        Live ->
            Color.lightGrey


type alias InteractionEvents a =
    { click : Int -> Msg
    , hover : Int -> Msg
    , leave : Msg
    , highlighted : Maybe a
    }


drawEventCells : (Int -> CellEvents Msg) -> Fragment.Span -> Case Cell -> Svg Msg
drawEventCells cellEvents span ruleCase =
    let
        offset: Offset
        offset =
            getCellOffset 1 0
    in
    List.range -span span
        |> List.map
            ( \i ->
                drawEventCell (addOffset offset (getCellOffset 0 (i + span))) <| cellEvents i
            )
        |> TypedSvg.g []


drawCell : Offset -> Cell -> Bool -> Svg Msg
drawCell offset cell isHighlighted =
    let
        attributes : List (Attribute msg)
        attributes =
            [ x <| px offset.x
            , y <| px offset.y
            , width  <| px <| toFloat cellDrawSize
            , height <| px <| toFloat cellDrawSize
            , fill <| Paint <|
                if isHighlighted then
                    getHighlightedCellColor cell
                else
                    getCellColor cell
            , stroke <| Paint Color.black
            , strokeWidth <| px 2
            ]
    in
    TypedSvg.rect
        attributes
        []


type alias CellEvents msg =
    { click : msg
    , hover : msg
    , leave : msg
    }


drawEventCell : Offset -> CellEvents Msg -> Svg Msg
drawEventCell offset cellEvents =
    let
        attributes : List (Attribute Msg)
        attributes =
            [ x <| px offset.x
            , y <| px offset.y
            , width  <| px <| toFloat cellDrawSize
            , height <| px <| toFloat cellDrawSize
            , fillOpacity <| Opacity 0
            , strokeOpacity <| Opacity 0
            , strokeWidth <| px 2
            , TypedSvg.Events.onClick cellEvents.click
            , TypedSvg.Events.onMouseOver cellEvents.hover
            , TypedSvg.Events.onMouseLeave cellEvents.leave
            ]
    in
    TypedSvg.rect
        attributes
        []


drawStates : Fragment.Span -> Maybe (Int -> Bool) -> Mobility -> Svg Msg
drawStates span highlighterOption mobility =
    let
        offset : Offset
        offset =
            getCellOffset 1 0
    in
    List.range -span span
        |> List.map
            (\s ->
                drawState (addOffset offset (getCellOffset 0 (span + s))) (Set.member s mobility) <|
                    case highlighterOption of
                        Nothing -> False
                        Just highlighter -> highlighter s
            )
        |> TypedSvg.g []


stateDrawSize : Int
stateDrawSize = (cellDrawSize // 2) - 2


drawState : Offset -> Bool -> Bool -> Svg Msg
drawState offset isActive isHighlighted =
    let
        attributes : List (Attribute Msg)
        attributes =
            [ cx <| px (offset.x + (toFloat cellDrawSize) / 2)
            , cy <| px (offset.y + (toFloat cellDrawSize) / 2)
            , r <| px <| toFloat stateDrawSize
            , fill <| 
                if isHighlighted || not isActive then
                    PaintNone
                else
                    Paint <| Color.black
            , stroke <|
                if isHighlighted || isActive then
                    Paint <| Color.black
                else
                    PaintNone
            , strokeWidth <| px <| 1
            ]
    in
    TypedSvg.circle
        attributes
        []


viewTape : Model -> Element Msg
viewTape model =
    E.el
        [ E.width E.fill
        , E.height E.fill
        ]
        ( E.html <|
            viewCustomElement
                [ mapCustomAttribute (Rule model.rule)
                , mapCustomAttribute (Status model.state.simulation)
                , mapCustomAttribute (CellSize model.settings.cellSize)
                ]
                [ Html.canvas
                    [ Html.Attributes.id "ma-canvas"
                    ] []
                , Html.canvas
                    [ Html.Attributes.id "ma-mobility"
                    ] []
                ]
        )


viewCustomElement : List (Html.Attribute a) -> List (Html a) -> Html a
viewCustomElement =
    Html.node "mobile-automata"


type AutomataAttribute
    = Rule (Rule Cell)
    | Status SimulationState
    | CellSize Int


mapCustomAttribute : AutomataAttribute -> Html.Attribute a
mapCustomAttribute attribute =
    let
        fields : { field : String, value : String }
        fields =
            case attribute of
                Rule rule ->
                    { field = "rule", value = Json.Encode.encode 0 <| encodeRule encodeCell rule }

                Status simulationState ->
                    let
                        stateString : String
                        stateString =
                            case simulationState of
                                Paused -> "paused"
                                Running -> "running"
                    in
                    { field = "state", value = stateString }

                CellSize cellSize ->
                    { field = "cell-size", value = String.fromInt cellSize }
    in
    Html.Attributes.attribute fields.field fields.value


-- HELPERS


toggleProgeny : Progeny Cell -> Int -> Result Error (Progeny Cell)
toggleProgeny progeny index =
    let
        currentProgenyAtIndex : Maybe Cell
        currentProgenyAtIndex =
            Debug.log "current progeny" (Fragment.getFragmentValue progeny index)
    in
    case currentProgenyAtIndex of
        Nothing ->
            Err (BadRuleUpdate "No progeny at supplied index")

        Just cell ->
            let
                newCell : Cell
                newCell =
                    case cell of
                        Dead -> Live
                        Live -> Dead
            in
            Fragment.updateFragment progeny index newCell
                |> Result.mapError mapFragmentError


toggleMobility : Mobility -> Int -> Result Error Mobility
toggleMobility mobility index =
    let
        test = Debug.log "triggered mobility" index
    in
    if Set.member index mobility then
        Ok (Set.remove index mobility)
    else
        Ok (Set.insert index mobility)


replaceMobility : Int -> Result Error Mobility
replaceMobility index =
    Ok (Set.singleton index)


mapFragmentError : Fragment.Error -> Error
mapFragmentError fragmentError =
    case fragmentError of
        Fragment.InvalidFragment description ->
            BadRuleUpdate description

        Fragment.InvalidIndex index ->
            BadRuleUpdate ("Not possible to update fragment at index: " ++ (String.fromInt index))


mapRuleError : Rule.Error -> Error
mapRuleError ruleError =
    case ruleError of
        Rule.InvalidProgeny description ->
            BadRuleUpdate description

        _ ->
            BadRuleUpdate ""


cellToInt : Cell -> Int
cellToInt cell =
    case cell of
        Dead -> 0
        Live -> 1


encodeCell : Cell -> Json.Encode.Value
encodeCell cell =
    Json.Encode.int <| cellToInt cell


caseToInt: Case Cell -> Int
caseToInt ruleCase =
    let
        combination : Combination Cell
        combination =
            getCombination ruleCase
    in
    combination
        |> Fragment.toList
        |> List.map cellToInt
        |> List.foldl (\i v -> i + 2*v) 0
