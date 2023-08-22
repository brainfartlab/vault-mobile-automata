module Rule exposing
    ( Rule, RuleType(..)
    , Error(..)
    , Case, Combination, Outcome, Progeny, Mobility
    , newRule
    , getSpan, getType, getCase, getCases
    , updateMobility, updateProgeny
    , encodeRule, ruleDecoder
    , getCombination, getProgeny, getMobility
    )

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline exposing (required, resolve)
import Json.Encode
import Set exposing (Set)

import Fragment exposing (Fragment, Error, Span, getFragments, newFragment, size, encodeFragment)


type RuleType
    = Simple
    | Extended
    | Generalized


type Error
    = InvalidMobility String
    | InvalidProgeny String
    | UnknownCombination
    | InvalidCombination


type alias Mobility = Set Int
type alias Combination a  = Fragment a
type alias Progeny a = Fragment a


type alias Outcome a =
    { progeny : Progeny a
    , mobility : Mobility
    }


type Case a =
    Case
        { outcome : Outcome a
        , combination : Combination a
        }


type Rule a
    = Rule RuleType Span (List (Case a))


newRule : RuleType -> Span -> List a -> a -> Rule a
newRule ruleType span symbols defaultSymbol =
    case ruleType of
        Simple ->
            newSimpleRule span symbols defaultSymbol

        Extended ->
            newExtendedRule span symbols defaultSymbol

        Generalized ->
            newGeneralizedRule span symbols defaultSymbol


newSimpleRule : Span -> List a -> a -> Rule a
newSimpleRule span symbols defaultSymbol =
    let
        combinations : List (Fragment a)
        combinations =
            getFragments span symbols

        defaultOutcome : Outcome a
        defaultOutcome =
            { progeny = newFragment defaultSymbol 0
            , mobility = Set.singleton -span
            }

        cases : List (Case a)
        cases =
            combinations
            |> List.map
                ( \c ->
                    Case
                        { outcome = defaultOutcome
                        , combination = c
                        }
                )
    in
    Rule Simple span cases


newExtendedRule : Span -> List a -> a -> Rule a
newExtendedRule span symbols defaultSymbol =
    let
        combinations : List (Fragment a)
        combinations =
            getFragments span symbols

        defaultOutcome : Outcome a
        defaultOutcome =
            { progeny = newFragment defaultSymbol span
            , mobility = Set.singleton 0
            }

        cases : List (Case a)
        cases =
            combinations
            |> List.map
                ( \c ->
                    Case
                        { outcome = defaultOutcome
                        , combination = c
                        }
                )
    in
    Rule Extended span cases


newGeneralizedRule : Span -> List a -> a -> Rule a
newGeneralizedRule span symbols defaultSymbol =
    let
        combinations : List (Fragment a)
        combinations =
            getFragments span symbols

        defaultOutcome : Outcome a
        defaultOutcome =
            { progeny = newFragment defaultSymbol 0
            , mobility = Set.singleton 0
            }

        cases : List (Case a)
        cases =
            combinations
            |> List.map
                ( \c ->
                    Case
                        { outcome = defaultOutcome
                        , combination = c
                        }
                )
    in
    Rule Generalized span cases


getCombination : Case a -> Combination a
getCombination (Case { outcome, combination }) =
    combination


getOutcome : Case a -> Outcome a
getOutcome (Case { outcome, combination }) =
    outcome


getProgeny : Case a -> Progeny a
getProgeny (Case { outcome, combination }) =
    outcome.progeny


getMobility : Case a -> Mobility
getMobility (Case { outcome, combination }) =
    outcome.mobility


updateAspect : Rule a -> Fragment a -> (RuleType -> Outcome a -> Result Error (Outcome a)) -> Result Error (Rule a)
updateAspect (Rule ruleType span cases) combination modifier =
    let
        isolatedCase : (List (Case a), List (Case a))
        isolatedCase =
            cases
            |> List.partition (\c -> getCombination c == combination)

        preservedCases : List (Case a)
        preservedCases =
            Tuple.second isolatedCase

        modifiedCaseResult : Result Error (Case a)
        modifiedCaseResult =
            Tuple.first isolatedCase
            |> List.head
            |> Result.fromMaybe UnknownCombination
            |> Result.andThen (\c -> modifier ruleType (getOutcome c))
            |> Result.andThen (\o -> Ok (Case { outcome = o, combination = combination }))
    in
    case modifiedCaseResult of
        Err error ->
            Err error

        Ok modifiedCase ->
            Ok (Rule ruleType span (modifiedCase::preservedCases))


updateProgeny : Combination a -> Progeny a -> Rule a -> Result Error (Rule a)
updateProgeny combination newProgeny rule =
    updateAspect rule combination (updateOutcomeProgeny newProgeny)


updateOutcomeProgeny : Progeny a -> RuleType -> Outcome a -> Result Error (Outcome a)
updateOutcomeProgeny newProgeny ruleType outcome =
    case ruleType of
        Simple ->
            if (Fragment.size newProgeny == 1) then
                Ok { outcome | progeny = newProgeny }
            else
                Err (InvalidProgeny "Progeny can only include middle cell")

        Extended ->
            if (Fragment.size newProgeny == Fragment.size outcome.progeny) then
                Ok { outcome | progeny = newProgeny }
            else
                Err (InvalidProgeny "Progeny must span the whole window")

        Generalized ->
            if (Fragment.size newProgeny == 1) then
                Ok { outcome | progeny = newProgeny }
            else
                Err (InvalidProgeny "Progeny can only include middle cell")


updateMobility : Combination a -> Mobility -> Rule a -> Result Error (Rule a)
updateMobility combination newMobility rule =
    let
        invalidMobilityIndices : Mobility
        invalidMobilityIndices =
            Set.filter (\m -> (abs m) > (getSpan rule)) newMobility
    in
    if (Set.size invalidMobilityIndices > 0) then
        Err (InvalidMobility "Indices out of rule span found!")
    else
        updateAspect rule combination (updateOutcomeMobility newMobility)


updateOutcomeMobility : Mobility -> RuleType -> Outcome a -> Result Error (Outcome a)
updateOutcomeMobility newMobility ruleType outcome =
    case ruleType of
        Simple ->
            if (Set.size newMobility /= 1) then
                Err (InvalidMobility "Cannot mobilize more than 1 cell")
            else
                if (Set.member 0 newMobility) then
                    Err (InvalidMobility "Cannot mobilize the same cell in the next iteration")
                else
                    Ok { outcome | mobility = newMobility }


        Extended ->
            if (Set.size newMobility == 1) then
                Ok { outcome | mobility = newMobility }
            else
                Err (InvalidMobility "Cannot mobilize more than 1 cell")

        Generalized ->
            Ok { outcome | mobility = newMobility }


getType : Rule a -> RuleType
getType (Rule ruleType _ _) =
    ruleType


ruleTypeToString : RuleType -> String
ruleTypeToString ruleType =
    case ruleType of
        Simple -> "simple"
        Extended -> "extended"
        Generalized -> "generalized"


getSpan : Rule a -> Span
getSpan (Rule _ span _) =
    span


getCases : Rule a -> List (Case a)
getCases (Rule _ _ cases) =
    cases


getCase : Combination a -> Rule a -> Result Error (Case a)
getCase combination (Rule _ span cases) =
    if (Fragment.size combination /= (2 * span + 1)) then
        Err InvalidCombination
    else
        cases
        |> List.filter (\c -> getCombination c == combination)
        |> List.head
        |> Result.fromMaybe UnknownCombination


encodeRule : (a -> Json.Encode.Value) -> Rule a -> Json.Encode.Value
encodeRule valueEncoder rule =
    Json.Encode.object
        [ ("rule-type", Json.Encode.string <| ruleTypeToString <| getType rule)
        , ("span", Json.Encode.int <| getSpan rule)
        , ("cases", Json.Encode.list (encodeCase valueEncoder) <| getCases rule)
        ]


encodeCase : (a -> Json.Encode.Value) -> Case a -> Json.Encode.Value
encodeCase valueEncoder (Case { outcome, combination }) =
    Json.Encode.object
        [ ("outcome", (encodeOutcome valueEncoder) outcome)
        , ("combination", (encodeFragment valueEncoder) combination)
        ]


encodeOutcome : (a -> Json.Encode.Value) -> Outcome a -> Json.Encode.Value
encodeOutcome valueEncoder { progeny, mobility } =
    Json.Encode.object
        [ ("progeny", (encodeFragment valueEncoder) progeny)
        , ("mobility", Json.Encode.set Json.Encode.int mobility)
        ]


fragmentDecoder : List a -> Json.Decode.Decoder (Fragment a)
fragmentDecoder cells =
    case Fragment.fromList cells of
        Ok fragment ->
            Json.Decode.succeed fragment

        Err error ->
            Json.Decode.fail ("Improper fragment: " ++ (String.fromInt (List.length cells)))


outcomeDecoder : Json.Decode.Decoder (List a) -> Json.Decode.Decoder (Outcome a)
outcomeDecoder symbolDecoder =
    Json.Decode.succeed Outcome
        |> required "progeny" (Json.Decode.andThen fragmentDecoder symbolDecoder)
        |> required "mobility" (Json.Decode.Extra.set Json.Decode.int)


caseDecoder : Json.Decode.Decoder (List a) -> Json.Decode.Decoder (Case a)
caseDecoder symbolDecoder =
    let
        toCaseDecoder : Combination a -> Outcome a -> Json.Decode.Decoder (Case a)
        toCaseDecoder combination outcome =
            Json.Decode.succeed <|
                Case
                    { combination = combination
                    , outcome = outcome
                    }
    in
    Json.Decode.succeed toCaseDecoder
        |> required "combination" (Json.Decode.andThen fragmentDecoder symbolDecoder)
        |> required "outcome" (outcomeDecoder symbolDecoder)
        |> resolve


ruleTypeDecoder : String -> Json.Decode.Decoder RuleType
ruleTypeDecoder ruleType =
    case ruleType of
        "simple" ->
            Json.Decode.succeed Simple

        "extended" ->
            Json.Decode.succeed Extended

        "generalized" ->
            Json.Decode.succeed Generalized

        _ ->
            Json.Decode.fail ("Unrecognized rule type: " ++ ruleType)


ruleDecoder : Json.Decode.Decoder (List a) -> Json.Decode.Decoder (Rule a)
ruleDecoder symbolDecoder =
    let
        toRuleDecoder : RuleType -> Span -> List (Case a) -> Json.Decode.Decoder (Rule a)
        toRuleDecoder ruleType span cases =
            Json.Decode.succeed <|
                Rule ruleType span cases
    in
    Json.Decode.succeed toRuleDecoder
        |> required "rule-type" (Json.Decode.andThen ruleTypeDecoder Json.Decode.string)
        |> required "span" Json.Decode.int
        |> required "cases" (Json.Decode.list <| caseDecoder symbolDecoder)
        |> resolve
