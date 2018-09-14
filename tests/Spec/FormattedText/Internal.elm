module Spec.FormattedText.Internal exposing (equalTests, overlapTests)

import Expect exposing (Expectation)
import FormattedText as FT
import FormattedText.Internal as Internal exposing (overlap)
import Test exposing (..)


overlapTests : Test
overlapTests =
    describe "FormattedText#overlap"
        [ test "ranges with different markup" <|
            \_ ->
                overlap { tag = "a", start = 0, end = 4 } { tag = "b", start = 1, end = 5 }
                    |> Expect.equal False
        , test "a to the left of b" <|
            \_ ->
                overlap { tag = "a", start = 0, end = 4 } { tag = "a", start = 5, end = 6 }
                    |> Expect.equal False
        , test "a to the right of b" <|
            \_ ->
                overlap { tag = "a", start = 5, end = 6 } { tag = "a", start = 0, end = 4 }
                    |> Expect.equal False
        , test "a touches b" <|
            \_ ->
                overlap { tag = "a", start = 0, end = 4 } { tag = "a", start = 4, end = 6 }
                    |> Expect.equal True
        , test "a to the left of b, partial intersecting" <|
            \_ ->
                overlap { tag = "a", start = 0, end = 4 } { tag = "a", start = 2, end = 6 }
                    |> Expect.equal True
        , test "a to the right of b, partial intersecting" <|
            \_ ->
                overlap { tag = "a", start = 2, end = 6 } { tag = "a", start = 0, end = 4 }
                    |> Expect.equal True
        , test "a encompassing in b" <|
            \_ ->
                overlap { tag = "a", start = 2, end = 6 } { tag = "a", start = 2, end = 4 }
                    |> Expect.equal True
        , test "a contained in b" <|
            \_ ->
                overlap { tag = "a", start = 2, end = 4 } { tag = "a", start = 2, end = 6 }
                    |> Expect.equal True
        , test "a equals b" <|
            \_ ->
                overlap { tag = "a", start = 2, end = 4 } { tag = "a", start = 2, end = 4 }
                    |> Expect.equal True
        ]



-- EQUAL TESTS


type Tag
    = First
    | Second


equalTests : Test
equalTests =
    let
        text1 =
            "abcdefghij 111"

        text2 =
            "abcdefghij 222"

        range tag start end =
            { tag = tag, start = start, end = end }

        rangesA1 =
            [ range First 0 4, range First 6 8 ]

        rangesA2 =
            [ range First 1 2, range First 3 5 ]

        rangesB1 =
            [ range Second 0 4, range Second 6 8 ]
    in
    describe "FormattedText#compareRanges"
        [ test "when formats are truely equal" <|
            \_ ->
                Internal.equal (FT.formattedText text1 rangesA1) (FT.formattedText text1 rangesA1)
                    |> Expect.equal True
        , test "when formats are equal in ranges, but not in text" <|
            \_ ->
                Internal.equal (FT.formattedText text1 rangesA1) (FT.formattedText text2 rangesA1)
                    |> Expect.equal False
        , test "when formats are equal in text, but not in ranges " <|
            \_ ->
                Internal.equal (FT.formattedText text1 rangesA1) (FT.formattedText text1 rangesA2)
                    |> Expect.equal False
        , test "when formats are equal in text, and ranges are equal in position, but not tag" <|
            \_ ->
                Internal.equal (FT.formattedText text1 rangesA1) (FT.formattedText text1 rangesB1)
                    |> Expect.equal False
        ]
