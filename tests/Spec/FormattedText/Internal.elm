module Spec.FormattedText.Internal exposing (overlapTests)

import Expect exposing (Expectation)
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
