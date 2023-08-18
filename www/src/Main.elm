module Main exposing (main)

import Debug
import Bitwise
import Browser
import Browser.Dom
import Color exposing (Color)
import Html exposing (Html)
import Set exposing (Set)
import Task

import Colors.Alpha as A
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
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


type RuleMsg
    = UpdateMobility (Int -> Result Error Mobility) (Combination Cell) Int
    | UpdateProgeny  (Int -> Result Error (Progeny Cell)) (Combination Cell) Int
    | UpdateType RuleType


updateRule : RuleMsg -> Rule Cell -> Result Rule.Error (Rule Cell)
updateRule msg rule =
    case msg of
        UpdateMobility mobilityModifier combination index ->
            let
                newMobilityResult : Result Error Mobility
                newMobilityResult = mobilityModifier index
            in
            case newMobilityResult of
                Ok newMobility ->
                    updateMobility combination newMobility rule

                Err error ->
                    Ok rule

        UpdateProgeny progenyModifier combination index ->
            let
                newProgenyResult : Result Error (Progeny Cell)
                newProgenyResult = progenyModifier index
            in
            case newProgenyResult of
                Ok newProgeny ->
                    updateProgeny combination newProgeny rule

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
    | UpdateRule RuleMsg
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


drawCase : Editing -> Fragment.Span -> RuleType -> Case Cell -> Element Msg
drawCase editing span ruleType ruleCase =
    let
        -- SVG viewbox
        drawDimensions : Offset
        drawDimensions =
            addOffset (Offset 2 2) (getCellOffset (2 * span + 1) 2)
    in
    E.html <|
        TypedSvg.svg
            [ width  <| px <| drawDimensions.y
            , height <| px <| drawDimensions.x
            , viewBox -1 -1 drawDimensions.y drawDimensions.x
            ]
            [ drawCombination ruleCase
            , drawProgeny editing span ruleCase
            , drawStates editing span ruleCase
            , drawEventCells editing span
            ]


drawCombination : Case Cell -> Svg Msg
drawCombination ruleCase =
    let
        offset : Offset
        offset = getCellOffset 0 0
    in
    drawCells offset (getCombination ruleCase) Nothing


drawProgeny : Editing -> Fragment.Span -> Case Cell -> Svg Msg
drawProgeny editing span ruleCase =
    let
        progeny : Progeny Cell
        progeny =
            getProgeny ruleCase

        offset: Offset
        offset =
            getCellOffset 1 (span - (Fragment.getSpan progeny))
    in
    drawCells offset (getProgeny ruleCase) <|
        case editing of
            Progeny highlightedOption ->
                let
                    -- helper function for progeny event
                    progenyModifier : Int -> Result Error (Progeny Cell)
                    progenyModifier index =
                        toggleProgeny (getProgeny ruleCase) index

                    -- progeny event
                    progenyMsg : Int -> Msg
                    progenyMsg index =
                        UpdateRule (UpdateProgeny progenyModifier (getCombination ruleCase) index)

                    -- hover event
                    hoverMsg : Int -> Msg
                    hoverMsg index =
                        UpdateSettings <| UpdateEditing <| Progeny <| Just (Highlighted ruleCase index)
                in
                Just
                    { click = progenyMsg
                    , hover = hoverMsg
                    , leave = UpdateSettings (UpdateEditing (Progeny Nothing))
                    , highlighted =
                        case highlightedOption of
                            Nothing ->
                                Nothing

                            Just highlighted ->
                                if highlighted.ruleCase == ruleCase then
                                    Just highlighted.index
                                 else
                                     Nothing
                    }

            Mobility highlightedOption ->
                let
                    -- helper function for progeny event
                    mobilityModifier : Int -> Result Error (Mobility)
                    mobilityModifier index =
                        toggleMobility (getMobility ruleCase) index

                    -- mobility event
                    mobilityMsg : Int -> Msg
                    mobilityMsg index =
                        UpdateRule (UpdateMobility mobilityModifier (getCombination ruleCase) index)

                    -- hover event
                    hoverMsg : Int -> Msg
                    hoverMsg index =
                        UpdateSettings <| UpdateEditing <| Mobility <| Just (Highlighted ruleCase index)
                in
                Just
                    { click = mobilityMsg
                    , hover = hoverMsg
                    , leave = UpdateSettings (UpdateEditing (Mobility Nothing))
                    , highlighted = Nothing
                    }


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


drawCells : Offset -> Fragment Cell -> Maybe (InteractionEvents Int) -> Svg Msg
drawCells offset cells interactionEventsOption =
    cells
        |> Fragment.toIndexedList
        |> List.indexedMap
            ( \i (index, cell) ->
                case interactionEventsOption of
                    Nothing ->
                        drawCell (addOffset offset (getCellOffset 0 i)) (Opacity 1) cell Nothing False

                    Just { click, hover, leave, highlighted } ->
                        let
                            events : { click : Msg, hover : Msg, leave : Msg }
                            events =
                                { click = click index
                                , hover = hover index
                                , leave = leave
                                }
                        in
                        case highlighted of
                            Nothing -> 
                                drawCell (addOffset offset (getCellOffset 0 i)) (Opacity 1) cell (Just events) False

                            Just highlightedIndex ->
                                drawCell (addOffset offset (getCellOffset 0 i)) (Opacity 1) cell (Just events) (index == highlightedIndex)
            )
        |> TypedSvg.g []


drawEventCells : Editing -> Fragment.Span -> Svg Msg
drawEventCells editing span =
    let
        offset: Offset
        offset =
            getCellOffset 1 0
    in
    List.range 0 (2 * span)
        |> List.map
            ( \i ->
                drawCell (addOffset offset (getCellOffset 0 i)) (Opacity 0) Dead Nothing False
            )
        |> TypedSvg.g []


drawCell : Offset -> Opacity -> Cell -> Maybe { click : Msg, hover : Msg, leave : Msg } -> Bool -> Svg Msg
drawCell offset opacity cell eventOptions isHighlighted =
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
            , fillOpacity opacity
            , stroke <| Paint Color.black
            , strokeOpacity opacity
            , strokeWidth <| px 2
            ]
    in
    TypedSvg.rect
        ( case eventOptions of
            Nothing ->
                attributes

            Just events ->
                List.append
                    [ TypedSvg.Events.onClick events.click
                    , TypedSvg.Events.onMouseOver events.hover
                    , TypedSvg.Events.onMouseLeave events.leave
                    ]
                    attributes
        )
        []


drawStates : Editing -> Fragment.Span -> Case Cell -> Svg Msg
drawStates editing span ruleCase =
    let
        offset : Offset
        offset =
            getCellOffset 1 0
    in
    getMobility ruleCase
        |> Set.toList
        |> List.map
            (\s ->
                case editing of
                    Progeny _ ->
                        drawState (addOffset offset (getCellOffset 0 (span + s))) False

                    Mobility highlightedOption ->
                        case highlightedOption of
                            Nothing ->
                                drawState (addOffset offset (getCellOffset 0 (span + s))) False

                            Just highlightedData ->
                                drawState (addOffset offset (getCellOffset 0 (span + s))) (s == highlightedData.index)
            )
        |> TypedSvg.g []


stateDrawSize : Int
stateDrawSize = (cellDrawSize // 2) - 2


drawState : Offset -> Bool -> Svg Msg
drawState offset isHighlighted =
    let
        attributes : List (Attribute Msg)
        attributes =
            [ cx <| px (offset.x + (toFloat cellDrawSize) / 2)
            , cy <| px (offset.y + (toFloat cellDrawSize) / 2)
            , r <| px <| toFloat stateDrawSize
            , fill <| 
                if isHighlighted then
                    PaintNone
                else
                    Paint <| Color.black
            , stroke <| Paint <| Color.black
            , strokeWidth <| px <| 1
            ]
    in
    TypedSvg.circle
        attributes
        []


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
    if Set.member index mobility then
        Ok (Set.remove index mobility)
    else
        Ok (Set.insert index mobility)


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
