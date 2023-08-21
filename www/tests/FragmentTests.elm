module FragmentTests exposing (newFragmentTests, updateFragmentTests, getFragmentTests, toIndexedListTests, fromListTests)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Json.Encode
import Fragment exposing (..)


type Cell
    = Live 
    | Dead
    | Zombie


newFragmentTests : Test
newFragmentTests =
    describe "newFragment"
        [ test "without sides" <|
            \_ ->
                newFragment Live 0
                    |> Fragment.toList
                    |> Expect.equal [Live]
        , test "with sides" <|
            \_ ->
                newFragment Dead 2
                    |> Fragment.toList
                    |> Expect.equal [Dead, Dead, Dead, Dead, Dead]
        ]


testFragment : Fragment Cell
testFragment = newFragment Dead 2


updateFragmentTests : Test
updateFragmentTests =
    describe "updateFragment"
        [ test "update left" <|
            \_ ->
                updateFragment testFragment -2 Live
                    |> Result.andThen (\f -> Ok(Fragment.toList f))
                    |> Expect.equal (Ok [Live, Dead, Dead, Dead, Dead])
        , test "update middle" <|
            \_ ->
                updateFragment testFragment 0 Live
                    |> Result.andThen (\f -> Ok(Fragment.toList f))
                    |> Expect.equal (Ok [Dead, Dead, Live, Dead, Dead])
        , test "update right" <|
            \_ ->
                updateFragment testFragment 1 Live
                    |> Result.andThen (\f -> Ok(Fragment.toList f))
                    |> Expect.equal (Ok [Dead, Dead, Dead, Live, Dead])
        ]


getFragmentTests : Test
getFragmentTests =
    describe "getFragments"
        [ test "window size 1" <|
            \_ ->
                getFragments 0 [Live, Dead]
                    |> List.map Fragment.toList
                    |> Expect.equalLists
                        [ [Live]
                        , [Dead]
                        ]
        , test "window size 3" <|
            \_ ->
                getFragments 1 [Live, Dead]
                    |> List.map Fragment.toList
                    |> Expect.equalLists
                        [ [Live, Live, Live]
                        , [Live, Live, Dead]
                        , [Live, Dead, Live]
                        , [Live, Dead, Dead]
                        , [Dead, Live, Live]
                        , [Dead, Live, Dead]
                        , [Dead, Dead, Live]
                        , [Dead, Dead, Dead]
                        ]
        , test "window size 3 multi-symbol" <|
            \_ ->
                getFragments 1 [Live, Dead, Zombie]
                    |> List.map Fragment.toList
                    |> Expect.equalLists
                        [ [Live, Live, Live]
                        , [Live, Live, Dead]
                        , [Live, Live, Zombie]
                        , [Live, Dead, Live]
                        , [Live, Dead, Dead]
                        , [Live, Dead, Zombie]
                        , [Live, Zombie, Live]
                        , [Live, Zombie, Dead]
                        , [Live, Zombie, Zombie]
                        , [Dead, Live, Live]
                        , [Dead, Live, Dead]
                        , [Dead, Live, Zombie]
                        , [Dead, Dead, Live]
                        , [Dead, Dead, Dead]
                        , [Dead, Dead, Zombie]
                        , [Dead, Zombie, Live]
                        , [Dead, Zombie, Dead]
                        , [Dead, Zombie, Zombie]
                        , [Zombie, Live, Live]
                        , [Zombie, Live, Dead]
                        , [Zombie, Live, Zombie]
                        , [Zombie, Dead, Live]
                        , [Zombie, Dead, Dead]
                        , [Zombie, Dead, Zombie]
                        , [Zombie, Zombie, Live]
                        , [Zombie, Zombie, Dead]
                        , [Zombie, Zombie, Zombie]
                        ]
        ]


toIndexedListTests : Test
toIndexedListTests =
    describe "toIndexedList"
        [ test "window size 1" <|
            \_ ->
                newFragment Dead 0
                    |> toIndexedList
                    |> Expect.equalLists
                        [ (0, Dead)
                        ]
        , test "window size 4" <|
            \_ ->
                newFragment Dead 4
                    |> toIndexedList
                    |> Expect.equalLists
                        [ (-4, Dead)
                        , (-3, Dead)
                        , (-2, Dead)
                        , (-1, Dead)
                        , (0, Dead)
                        , (1, Dead)
                        , (2, Dead)
                        , (3, Dead)
                        , (4, Dead)
                        ]
        ]


testEncoder : Cell -> Json.Encode.Value
testEncoder cell =
    case cell of
        Live ->
            Json.Encode.int 1

        Dead ->
            Json.Encode.int 0

        Zombie ->
            Json.Encode.int 2


encodeFragmentTests : Test
encodeFragmentTests =
    describe "encodeFragment"
        [ test "encode fragment" <|
            \_ ->
                encodeFragment testEncoder (newFragment Dead 2)
                    |> Json.Encode.encode 0
                    |> Expect.equal "[0,0,0,0,0]"
        ]


fromListTests : Test
fromListTests =
    describe "fromList"
        [ test "without sides" <|
            \_ ->
                fromList [Live]
                    |> Result.andThen (\l -> Ok (Fragment.toList l))
                    |> Expect.equal (Ok [Live])
        , test "with sides" <|
            \_ ->
                fromList [Live, Live, Live]
                    |> Result.andThen (\l -> Ok (Fragment.toList l))
                    |> Expect.equal (Ok [Live, Live, Live])
        , test "even" <|
            \_ ->
                fromList [Live, Live]
                    |> Expect.err
        ]
