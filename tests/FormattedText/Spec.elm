module FormattedText.Spec exposing (spec)

import Compare
import Dict
import Dict.Extra
import EqualCheck exposing (EqualCheck)
import Expect exposing (Expectation)
import FormattedText as FormattedText exposing (FormattedText, Range)
import FormattedText.Fuzz as Fuzz
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Test exposing (..)


spec : Test
spec =
    describe "FormattedText"
        [ fuzz formattedText "Formatted text ranges with equal markup never overlap" <|
            \formatted ->
                FormattedText.ranges formatted
                    |> Dict.Extra.groupBy .tag
                    |> Dict.values
                    |> assertForAll rangesDontOverlap
        , test "Ranges that do not overlap are not merged into one" <|
            \_ ->
                let
                    text =
                        "abcdefghij"

                    ranges =
                        [ { tag = "a", start = 0, end = 4 }, { tag = "a", start = 6, end = 8 } ]
                in
                FormattedText.formattedText text ranges
                    |> FormattedText.ranges
                    |> equalRanges ranges
        , fuzz2 string string ".append" <|
            \a b ->
                FormattedText.append (FormattedText.fromString a) (FormattedText.fromString b)
                    |> equalFormattedTexts (FormattedText.fromString <| a ++ b)
        , fuzz (list string) ".concat" <|
            \xs ->
                List.map FormattedText.fromString xs
                    |> FormattedText.concat
                    |> equalFormattedTexts (FormattedText.fromString <| String.concat xs)
        , fuzz string "fromString and text are duals" <|
            \text ->
                FormattedText.fromString text
                    |> FormattedText.text
                    |> Expect.equal text
        , test ".chunks" <|
            \_ ->
                FormattedText.formattedText
                    "foo bar baz"
                    [ { tag = "a", start = 0, end = 3 }, { tag = "b", start = 8, end = 11 } ]
                    |> FormattedText.chunks (,)
                    |> Expect.equal [ ( "foo", [ "a" ] ), ( " bar ", [] ), ( "baz", [ "b" ] ) ]
        , fuzz formattedText "chunks and unchunk are duals" <|
            \formatted ->
                formatted
                    |> FormattedText.chunks (,)
                    |> FormattedText.unchunk
                    |> equalFormattedTexts formatted
        , describe ".reverse"
            [ fuzz formattedText "reversing twice is equivalent to doing nothing" <|
                \formatted ->
                    formatted
                        |> FormattedText.reverse
                        |> FormattedText.reverse
                        |> equalFormattedTexts formatted
            , fuzz formattedText "works the same as String.reverse" <|
                \formatted ->
                    formatted
                        |> FormattedText.reverse
                        |> FormattedText.text
                        |> Expect.equal (formatted |> FormattedText.text |> String.reverse)
            ]
        , fuzz2 formattedText (intRange 0 5) ".repeat" <|
            \formatted n ->
                formatted
                    |> FormattedText.repeat n
                    |> FormattedText.text
                    |> Expect.equal (formatted |> FormattedText.text |> String.repeat n)
        , describe ".cons"
            [ fuzz formattedText "works the same as String.cons" <|
                \formatted ->
                    formatted
                        |> FormattedText.cons 'a'
                        |> FormattedText.text
                        |> Expect.equal (formatted |> FormattedText.text |> String.cons 'a')
            ]
        , describe ".uncons"
            [ fuzz formattedText "works the same as String.uncons" <|
                \formatted ->
                    formatted
                        |> FormattedText.uncons
                        |> Maybe.map (Tuple.mapSecond FormattedText.text)
                        |> Expect.equal (formatted |> FormattedText.text |> String.uncons)
            , fuzz formattedText "is the opposite of .cons" <|
                \formatted ->
                    formatted
                        |> FormattedText.cons 'a'
                        |> FormattedText.uncons
                        |> just
                            (Expect.all
                                [ Tuple.first >> Expect.equal 'a'
                                , Tuple.second >> equalFormattedTexts formatted
                                ]
                            )
            ]
        , describe ".left"
            [ fuzz2 formattedText formattedText "exactly replicates part of a formated text" <|
                \first last ->
                    FormattedText.append first last
                        |> FormattedText.left (FormattedText.length first)
                        |> equalFormattedTexts first
            , fuzz2 formattedText int "works the same as String.left" <|
                \formatted n ->
                    FormattedText.left n formatted
                        |> FormattedText.text
                        |> Expect.equal (FormattedText.text formatted |> String.left n)
            ]
        , describe ".right"
            [ fuzz2 formattedText formattedText "exactly replicates part of a formatted text" <|
                \first last ->
                    FormattedText.append first last
                        |> FormattedText.right (FormattedText.length last)
                        |> equalFormattedTexts last
            , fuzz2 formattedText int "works the same as String.right" <|
                \formatted n ->
                    FormattedText.right n formatted
                        |> FormattedText.text
                        |> Expect.equal (FormattedText.text formatted |> String.right n)
            ]
        , describe ".dropLeft"
            [ fuzz2 formattedText formattedText "exactly replicates part of a formated text" <|
                \first last ->
                    FormattedText.append first last
                        |> FormattedText.dropLeft (FormattedText.length first)
                        |> equalFormattedTexts last
            , fuzz2 formattedText int "works the same as String.dropLeft" <|
                \formatted n ->
                    FormattedText.dropLeft n formatted
                        |> FormattedText.text
                        |> Expect.equal (FormattedText.text formatted |> String.dropLeft n)
            ]
        , describe ".dropRight"
            [ fuzz2 formattedText formattedText "exactly replicates part of a formated text" <|
                \first last ->
                    FormattedText.append first last
                        |> FormattedText.dropRight (FormattedText.length last)
                        |> equalFormattedTexts first
            , fuzz2 formattedText int "works the same as String.dropRight" <|
                \formatted n ->
                    FormattedText.dropRight n formatted
                        |> FormattedText.text
                        |> Expect.equal (FormattedText.text formatted |> String.dropRight n)
            ]
        , describe ".slice"
            [ fuzz3 formattedText formattedText formattedText "exactly replicated part of a formatted text" <|
                \first middle last ->
                    FormattedText.concat [ first, middle, last ]
                        |> FormattedText.slice
                            (FormattedText.length first)
                            (FormattedText.length first + FormattedText.length middle)
                        |> equalFormattedTexts middle
            , fuzz3 formattedText int int "works the same as String.slice" <|
                \formatted start end ->
                    FormattedText.slice start end formatted
                        |> FormattedText.text
                        |> Expect.equal (FormattedText.text formatted |> String.slice start end)
            ]
        , describe ".split"
            [ fuzz2 string formattedText "works the same as String.split" <|
                \splitter formatted ->
                    FormattedText.split splitter formatted
                        |> List.map FormattedText.text
                        |> Expect.equal (FormattedText.text formatted |> String.split splitter)
            ]
        ]


just : (a -> Expectation) -> (Maybe a -> Expectation)
just expectation maybe =
    case maybe of
        Nothing ->
            Expect.fail "Expected a Just but got Nothing"

        Just x ->
            expectation x


equalFormattedTexts : EqualCheck (FormattedText markup)
equalFormattedTexts formattedA formattedB =
    formattedB
        |> Expect.all
            [ FormattedText.ranges >> equalRanges (FormattedText.ranges formattedA)
            , FormattedText.text >> Expect.equal (FormattedText.text formattedA)
            ]


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


formattedText : Fuzzer (FormattedText Fuzz.Tag)
formattedText =
    Fuzz.formattedText Fuzz.tag


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
