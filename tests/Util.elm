module Util exposing (..)

{-| Shared helper functions for tests.

Some of these methods might fit nicely into libraries.

-}

import Compare
import EqualCheck exposing (EqualCheck)
import Expect exposing (Expectation)
import FormattedText as FT exposing (FormattedText, Range)
import Fuzz exposing (Fuzzer, int, intRange, list, string)


just : (a -> Expectation) -> (Maybe a -> Expectation)
just expectation maybe =
    case maybe of
        Nothing ->
            Expect.fail "Expected a Just but got Nothing"

        Just x ->
            expectation x


equalRanges : EqualCheck (List (Range markup))
equalRanges rangesA rangesB =
    let
        orderRanges : Compare.Comparator (Range markup)
        orderRanges =
            Compare.concat [ Compare.by .start, Compare.by (.tag >> toString) ]
    in
    EqualCheck.listContents orderRanges rangesA rangesB


rangesDontOverlap : List (Range markup) -> Expectation
rangesDontOverlap ranges =
    let
        bounds : Range markup -> List Int
        bounds { start, end } =
            [ start, end ]
    in
    ranges
        |> List.sortBy .start
        |> List.concatMap bounds
        |> (\bounds -> bounds |> Expect.equal (List.sort bounds))


{-| Whereas `Expect.all` allows you to run multiple assertions against a single piece of data,
`assertForall` allows you to run a single assertion against multiple pieces of data.
-}
assertForAll : (a -> Expect.Expectation) -> List a -> Expect.Expectation
assertForAll testFn cases =
    case cases of
        -- Expect.all will fail an empty list of expectations, which is not what we want here.
        [] ->
            Expect.pass

        nonEmptyCases ->
            nonEmptyCases
                |> List.map (\singleCase _ -> testFn singleCase)
                |> flip Expect.all ()


expectAnd : Expect.Expectation -> Expect.Expectation -> Expect.Expectation
expectAnd a b =
    Expect.all [ always a, always b ] ()


shortList : Fuzzer a -> Fuzzer (List a)
shortList fuzzer =
    Fuzz.oneOf
        [ Fuzz.constant []
        , Fuzz.map (\a -> [ a ]) fuzzer
        , Fuzz.map2 (\a b -> [ a, b ]) fuzzer fuzzer
        ]


atLeastOneList : Fuzzer a -> Fuzzer (List a)
atLeastOneList fuzzer =
    Fuzz.oneOf
        [ Fuzz.map (\a -> [ a ]) fuzzer
        , Fuzz.map2 (\a b -> [ a, b ]) fuzzer fuzzer
        ]


{-| TODO: Move this into EqualCheck
-}
equalLists : EqualCheck a -> List a -> List a -> Expect.Expectation
equalLists check xs ys =
    case ( xs, ys ) of
        ( [], [] ) ->
            Expect.pass

        ( x :: xs, y :: ys ) ->
            expectAnd (check x y) (equalLists check xs ys)

        _ ->
            Expect.fail <| "Lists are not the same: " ++ toString ( xs, ys )
