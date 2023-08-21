module Fragment exposing
    ( Fragment, Error(..), Span
    , newFragment, updateFragment, getFragmentValue
    , getFragments
    , fromList, toList, toIndexedList, size, getSpan
    , encodeFragment
    )

import Array exposing (Array)
import Json.Encode

type alias Span = Int

type alias Side a = Array a

type Fragment a
    = Fragment (Side a) a (Side a)

type Error
    = InvalidFragment String
    | InvalidIndex Int


newFragment : a -> Span -> Fragment a
newFragment defaultSymbol span =
    if span == 0 then
        Fragment Array.empty defaultSymbol Array.empty
    else
        let
            side : Side a
            side = Array.repeat span defaultSymbol
        in
        Fragment side defaultSymbol side


getFragmentValue : Fragment a -> Int -> Maybe a
getFragmentValue (Fragment left middle right) index =
    let
        relativeIndex: Int
        relativeIndex = (abs index) - 1
    in
    case (compare index 0) of
        LT ->
            Array.get relativeIndex left

        EQ ->
            Just middle

        GT ->
            Array.get relativeIndex right


getSpan : Fragment a -> Span
getSpan (Fragment left _ _) =
    Array.length left


updateFragment : Fragment a -> Int -> a -> Result Error (Fragment a)
updateFragment (Fragment left middle right) index symbol =
    let
        relativeIndex: Int
        relativeIndex = (abs index) - 1
    in
    if relativeIndex > (Array.length left) then
        Err (InvalidIndex index)
    else
        case (compare index 0) of
            LT ->
                Ok (Fragment (Array.set relativeIndex symbol left) middle right)

            EQ ->
                Ok (Fragment left symbol right)

            GT ->
                Ok (Fragment left middle (Array.set relativeIndex symbol right))


getFragments : Span -> List a -> List (Fragment a)
getFragments span symbols =
    if span == 0 then
        symbols
        |> List.map (\s -> (newFragment s 0))
    else
        let
            sides : List (Side a)
            sides =
                permutations span symbols
        in
        sides
        |> List.map (\left ->
            symbols
            |> List.map (\middle ->
                sides
                |> List.map (Fragment left middle)
            )
            |> List.concat
        )
        |> List.concat


permutations : Span -> List a -> List (Side a)
permutations span symbols =
    if span > 1 then
        product (permutations (span - 1) symbols) symbols
    else
        symbols
        |> List.map (Array.fromList << List.singleton)


product : List (Side a) -> List a -> List (Side a)
product sides symbols =
    symbols
    |> List.map (augment sides)
    |> List.concat


augment : List (Side a) -> a -> List (Side a)
augment combinations symbol =
    combinations
    |> List.map (Array.push symbol)


toList : Fragment a -> List a
toList (Fragment left middle right) =
    List.concat [List.reverse (Array.toList left), [middle], Array.toList right]


fromList : List a -> Result Error (Fragment a)
fromList cells =
    case cells of
        [middle] ->
            Ok (Fragment Array.empty middle Array.empty)

        _ ->
            case modBy 2 (List.length cells) of
                0 ->
                    Err (InvalidFragment "Even number of cells")

                1 ->
                    let
                        span: Span
                        span =
                            ((List.length cells) - 1) // 2

                        left : Array a
                        left = Array.fromList <| List.take span cells

                        middleOption : Maybe a
                        middleOption =
                            List.drop 2 cells |> List.head

                        right : Array a
                        right = Array.fromList <| List.drop (span + 1) cells
                    in
                    case middleOption of
                        Just middle ->
                            Ok (Fragment left middle right)

                        Nothing ->
                            Err (InvalidFragment "Unexpected error")

                _ ->
                    Err (InvalidFragment "Unexpected error")


toIndexedList : Fragment a -> List (Int, a)
toIndexedList fragment =
    let
        span : Span
        span = getSpan fragment
    in
    toList fragment
        |> List.indexedMap (\i v -> (i - span, v))


size : Fragment a -> Int
size (Fragment left middle right) =
    (Array.length left) + 1 + (Array.length right)


encodeFragment : (a -> Json.Encode.Value) -> Fragment a -> Json.Encode.Value
encodeFragment valueEncoder fragment =
    Json.Encode.list valueEncoder (toList fragment)
