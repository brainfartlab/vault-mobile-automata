module RuleTests exposing (newRuleTests, updateProgenyTests, updateMobilityTests, encodeRuleTests)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Json.Encode
import Set exposing (Set)

import Fragment
import Rule exposing (..)


type Cell
    = Live
    | Dead


newRuleTests : Test
newRuleTests =
    describe "newRule"
        [ test "simple rule" <|
            \_ ->
                newRule Simple 2 [Live, Dead] Dead
                    |> getCases
                    |> List.head
                    |> Maybe.andThen (\c -> Just (Fragment.toList (getProgeny c)))
                    |> Expect.equal (Just [Dead])
        , test "extended rule" <|
            \_ ->
                newRule Extended 2 [Live, Dead] Dead
                    |> getCases
                    |> List.head
                    |> Maybe.andThen (\c -> Just (Fragment.toList (getProgeny c)))
                    |> Expect.equal (Just [Dead, Dead, Dead, Dead, Dead])
        , test "generalized rule" <|
            \_ ->
                newRule Generalized 2 [Live, Dead] Dead
                    |> getCases
                    |> List.head
                    |> Maybe.andThen (\c -> Just (Fragment.toList (getProgeny c)))
                    |> Expect.equal (Just [Dead])
        ]


updateProgenyTests : Test
updateProgenyTests =
    let
        simpleRule : Rule Cell
        simpleRule = newRule Simple 2 [Live, Dead] Dead

        extendedRule : Rule Cell
        extendedRule = newRule Extended 2 [Live, Dead] Dead

        generalizedRule : Rule Cell
        generalizedRule = newRule Generalized 2 [Live, Dead] Dead

        newSimpleProgeny : Progeny Cell
        newSimpleProgeny = Fragment.newFragment Live 0

        newExtendedProgeny : Progeny Cell
        newExtendedProgeny = Fragment.newFragment Live 2
    in
    describe "updateProgeny"
        [ fuzz (Fuzz.oneOfValues (getCases simpleRule)) "simple rule" <|
            \rc ->
                updateProgeny (getCombination rc) newSimpleProgeny simpleRule
                    |> Result.andThen (\r -> getCase (getCombination rc) r)
                    |> Result.andThen (\c -> Ok (getProgeny c))
                    |> Expect.equal (Ok newSimpleProgeny)
        , fuzz (Fuzz.oneOfValues (getCases simpleRule)) "simple rule invalid progeny" <|
            \rc ->
                updateProgeny (getCombination rc) newExtendedProgeny simpleRule
                    |> Expect.err
        , fuzz (Fuzz.oneOfValues (getCases extendedRule)) "extended rule" <|
            \rc ->
                updateProgeny (getCombination rc) newExtendedProgeny extendedRule
                    |> Result.andThen (\r -> getCase (getCombination rc) r)
                    |> Result.andThen (\c -> Ok (getProgeny c))
                    |> Expect.equal (Ok newExtendedProgeny)
        , fuzz (Fuzz.oneOfValues (getCases extendedRule)) "extended rule invalid progeny" <|
            \rc ->
                updateProgeny (getCombination rc) newSimpleProgeny extendedRule
                    |> Expect.err
        , fuzz (Fuzz.oneOfValues (getCases generalizedRule)) "generalized rule" <|
            \rc ->
                updateProgeny (getCombination rc) newSimpleProgeny generalizedRule
                    |> Result.andThen (\r -> getCase (getCombination rc) r)
                    |> Result.andThen (\c -> Ok (getProgeny c))
                    |> Expect.equal (Ok newSimpleProgeny)
        , fuzz (Fuzz.oneOfValues (getCases generalizedRule)) "generalized rule invalid progeny" <|
            \rc ->
                updateProgeny (getCombination rc) newExtendedProgeny generalizedRule
                    |> Expect.err
        ]


updateMobilityTests : Test
updateMobilityTests =
    let
        simpleRule : Rule Cell
        simpleRule = newRule Simple 2 [Live, Dead] Dead

        extendedRule : Rule Cell
        extendedRule = newRule Extended 2 [Live, Dead] Dead

        generalizedRule : Rule Cell
        generalizedRule = newRule Generalized 2 [Live, Dead] Dead

        newSimpleMobility : Mobility
        newSimpleMobility = Set.singleton -1

        newExtendedMobility : Mobility
        newExtendedMobility = Set.singleton 0

        newGeneralizedMobility : Mobility
        newGeneralizedMobility = Set.fromList [1, 0]
    in
    describe "updateMobility"
        [ fuzz (Fuzz.oneOfValues (getCases simpleRule)) "simple rule" <|
            \rc ->
                updateMobility (getCombination rc) newSimpleMobility simpleRule
                    |> Result.andThen (\r -> getCase (getCombination rc) r)
                    |> Result.andThen (\c -> Ok (getMobility c))
                    |> Expect.equal (Ok newSimpleMobility)
        , fuzz (Fuzz.oneOfValues (getCases simpleRule)) "simple rule invalid progeny" <|
            \rc ->
                updateMobility (getCombination rc) (Set.singleton 0) simpleRule
                    |> Expect.err
        , fuzz (Fuzz.oneOfValues (getCases extendedRule)) "extended rule" <|
            \rc ->
                updateMobility (getCombination rc) newExtendedMobility extendedRule
                    |> Result.andThen (\r -> getCase (getCombination rc) r)
                    |> Result.andThen (\c -> Ok (getMobility c))
                    |> Expect.equal (Ok newExtendedMobility)
        , fuzz (Fuzz.oneOfValues (getCases extendedRule)) "extended rule invalid progeny" <|
            \rc ->
                updateMobility (getCombination rc) newGeneralizedMobility extendedRule
                    |> Expect.err
        , fuzz (Fuzz.oneOfValues (getCases generalizedRule)) "generalized rule" <|
            \rc ->
                updateMobility (getCombination rc) newGeneralizedMobility generalizedRule
                    |> Result.andThen (\r -> getCase (getCombination rc) r)
                    |> Result.andThen (\c -> Ok (getMobility c))
                    |> Expect.equal (Ok newGeneralizedMobility)
        , fuzz (Fuzz.oneOfValues (getCases generalizedRule)) "generalized rule invalid progeny" <|
            \rc ->
                updateMobility (getCombination rc) (Set.singleton -3) generalizedRule
                    |> Expect.err
        ]


testEncoder : Cell -> Json.Encode.Value
testEncoder cell =
    case cell of
        Live ->
            Json.Encode.int 1

        Dead ->
            Json.Encode.int 0


encodeRuleTests : Test
encodeRuleTests =
    describe "encodeRule"
        [ test "simple rule" <|
            \_ ->
                encodeRule testEncoder (newRule Simple 1 [Live, Dead] Live)
                    |> Json.Encode.encode 4
                    |> Expect.equal testEncodedRule
        ]


testEncodedRule : String
testEncodedRule = """[
    {
        "outcome": {
            "progeny": [
                1
            ],
            "mobility": [
                -1
            ]
        },
        "combination": [
            1,
            1,
            1
        ]
    },
    {
        "outcome": {
            "progeny": [
                1
            ],
            "mobility": [
                -1
            ]
        },
        "combination": [
            1,
            1,
            0
        ]
    },
    {
        "outcome": {
            "progeny": [
                1
            ],
            "mobility": [
                -1
            ]
        },
        "combination": [
            1,
            0,
            1
        ]
    },
    {
        "outcome": {
            "progeny": [
                1
            ],
            "mobility": [
                -1
            ]
        },
        "combination": [
            1,
            0,
            0
        ]
    },
    {
        "outcome": {
            "progeny": [
                1
            ],
            "mobility": [
                -1
            ]
        },
        "combination": [
            0,
            1,
            1
        ]
    },
    {
        "outcome": {
            "progeny": [
                1
            ],
            "mobility": [
                -1
            ]
        },
        "combination": [
            0,
            1,
            0
        ]
    },
    {
        "outcome": {
            "progeny": [
                1
            ],
            "mobility": [
                -1
            ]
        },
        "combination": [
            0,
            0,
            1
        ]
    },
    {
        "outcome": {
            "progeny": [
                1
            ],
            "mobility": [
                -1
            ]
        },
        "combination": [
            0,
            0,
            0
        ]
    }
]"""
