module Spec.FormattedText exposing (spec)

import Dict
import Dict.Extra
import Expect exposing (Expectation)
import FormattedText as FT exposing (FormattedText, Range)
import FormattedText.Fuzz exposing (Markup(Yellow), customFormattedText, formattedText, markup)
import FormattedText.Regex as FTRegex
import Fuzz exposing (Fuzzer, char, int, intRange, list, string)
import Regex
import Test exposing (..)
import Util exposing (assertForAll, atLeastOneList, equalFormattedTexts, equalLists, equalRanges, just, rangesDontOverlap, shortList)


spec : Test
spec =
    describe "FormattedText"
        [ fuzz formattedText "Formatted text ranges with equal markup never overlap" <|
            \formatted ->
                FT.ranges formatted
                    |> Dict.Extra.groupBy (.tag >> toString)
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
                FT.formattedText text ranges
                    |> FT.ranges
                    |> equalRanges ranges
        , fuzz2 string string ".append" <|
            \a b ->
                FT.append (FT.fromString a) (FT.fromString b)
                    |> equalFormattedTexts (FT.fromString <| a ++ b)
        , fuzz (list string) ".concat" <|
            \xs ->
                List.map FT.fromString xs
                    |> FT.concat
                    |> equalFormattedTexts (FT.fromString <| String.concat xs)
        , fuzz string "fromString and text are duals" <|
            \text ->
                FT.fromString text
                    |> FT.text
                    |> Expect.equal text
        , test ".chunks" <|
            \_ ->
                FT.formattedText
                    "foo bar baz"
                    [ { tag = "a", start = 0, end = 3 }, { tag = "b", start = 8, end = 11 } ]
                    |> FT.chunks (,)
                    |> Expect.equal [ ( "foo", [ "a" ] ), ( " bar ", [] ), ( "baz", [ "b" ] ) ]
        , fuzz formattedText "chunks and unchunk are duals" <|
            \formatted ->
                formatted
                    |> FT.chunks (,)
                    |> FT.unchunk
                    |> equalFormattedTexts formatted
        , describe ".reverse"
            [ fuzz formattedText "reversing twice is equivalent to doing nothing" <|
                \formatted ->
                    formatted
                        |> FT.reverse
                        |> FT.reverse
                        |> equalFormattedTexts formatted
            , fuzz formattedText "works the same as String.reverse" <|
                \formatted ->
                    formatted
                        |> FT.reverse
                        |> FT.text
                        |> Expect.equal (formatted |> FT.text |> String.reverse)
            ]
        , fuzz2 formattedText (intRange 0 5) ".repeat" <|
            \formatted n ->
                formatted
                    |> FT.repeat n
                    |> FT.text
                    |> Expect.equal (formatted |> FT.text |> String.repeat n)
        , describe ".cons"
            [ fuzz formattedText "works the same as String.cons" <|
                \formatted ->
                    formatted
                        |> FT.cons 'a'
                        |> FT.text
                        |> Expect.equal (formatted |> FT.text |> String.cons 'a')
            ]
        , describe ".uncons"
            [ fuzz formattedText "works the same as String.uncons" <|
                \formatted ->
                    formatted
                        |> FT.uncons
                        |> Maybe.map (Tuple.mapSecond FT.text)
                        |> Expect.equal (formatted |> FT.text |> String.uncons)
            , fuzz formattedText "is the opposite of .cons" <|
                \formatted ->
                    formatted
                        |> FT.cons 'a'
                        |> FT.uncons
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
                    FT.append first last
                        |> FT.left (FT.length first)
                        |> equalFormattedTexts first
            , fuzz2 formattedText int "works the same as String.left" <|
                \formatted n ->
                    FT.left n formatted
                        |> FT.text
                        |> Expect.equal (FT.text formatted |> String.left n)
            ]
        , describe ".right"
            [ fuzz2 formattedText formattedText "exactly replicates part of a formatted text" <|
                \first last ->
                    FT.append first last
                        |> FT.right (FT.length last)
                        |> equalFormattedTexts last
            , fuzz2 formattedText int "works the same as String.right" <|
                \formatted n ->
                    FT.right n formatted
                        |> FT.text
                        |> Expect.equal (FT.text formatted |> String.right n)
            ]
        , describe ".dropLeft"
            [ fuzz2 formattedText formattedText "exactly replicates part of a formated text" <|
                \first last ->
                    FT.append first last
                        |> FT.dropLeft (FT.length first)
                        |> equalFormattedTexts last
            , fuzz2 formattedText int "works the same as String.dropLeft" <|
                \formatted n ->
                    FT.dropLeft n formatted
                        |> FT.text
                        |> Expect.equal (FT.text formatted |> String.dropLeft n)
            ]
        , describe ".dropRight"
            [ fuzz2 formattedText formattedText "exactly replicates part of a formated text" <|
                \first last ->
                    FT.append first last
                        |> FT.dropRight (FT.length last)
                        |> equalFormattedTexts first
            , fuzz2 formattedText int "works the same as String.dropRight" <|
                \formatted n ->
                    FT.dropRight n formatted
                        |> FT.text
                        |> Expect.equal (FT.text formatted |> String.dropRight n)
            ]
        , describe ".slice"
            [ fuzz3 formattedText formattedText formattedText "exactly replicated part of a formatted text" <|
                \first middle last ->
                    FT.concat [ first, middle, last ]
                        |> FT.slice
                            (FT.length first)
                            (FT.length first + FT.length middle)
                        |> equalFormattedTexts middle
            , fuzz3 formattedText int int "works the same as String.slice" <|
                \formatted start end ->
                    FT.slice start end formatted
                        |> FT.text
                        |> Expect.equal (FT.text formatted |> String.slice start end)
            ]
        , describe ".split"
            [ fuzz2 string formattedText "works the same as String.split" <|
                \splitter formatted ->
                    FT.split splitter formatted
                        |> List.map FT.text
                        |> Expect.equal (FT.text formatted |> String.split splitter)

            -- Manual test for a use case that gets generated rarely by the fuzzer test above.
            , test "splitter and text are the same" <|
                \_ ->
                    FT.split "#" (FT.fromString "#")
                        |> List.map FT.text
                        |> Expect.equal [ "", "" ]
            ]
        , describe ".join"
            [ fuzz2 formattedText (shortList formattedText) "works the same as String.join" <|
                \joiner parts ->
                    FT.join joiner parts
                        |> FT.text
                        |> Expect.equal (String.join (FT.text joiner) (List.map FT.text parts))
            ]
        , fuzz (atLeastOneList formattedText) ".join and .split are duals" <|
            \parts ->
                let
                    -- As of writing, string fuzzers will not generate these characters,
                    -- so we can be sure the fuzzer won't accidentally insert them into the parts.
                    splitter : String
                    splitter =
                        "😁🐢"
                in
                FT.join (FT.fromString splitter) parts
                    |> FT.split splitter
                    |> equalLists equalFormattedTexts parts
        , describe ".lines"
            [ fuzz formattedText "works the same as String.lines" <|
                \formatted ->
                    FT.lines formatted
                        |> List.map FT.text
                        |> Expect.equal (FT.text formatted |> String.lines)
            ]
        , describe ".words"
            [ fuzz formattedText "works the same as String.words" <|
                \formatted ->
                    FT.words formatted
                        |> List.map FT.text
                        |> Expect.equal (FT.text formatted |> String.words)
            , fuzz formattedText ".words >> .concatenate removes all whitespace" <|
                \formatted ->
                    let
                        noWhitespace =
                            FTRegex.replace
                                Regex.All
                                (Regex.regex "\\s+")
                                (always FT.empty)
                                formatted
                    in
                    FT.words formatted
                        |> FT.concat
                        |> equalFormattedTexts noWhitespace
            ]
        , describe ".trim"
            [ fuzz formattedText "works the same as String.trim" <|
                \formatted ->
                    FT.trim formatted
                        |> FT.text
                        |> Expect.equal (FT.text formatted |> String.trim)
            ]
        , describe ".trimLeft"
            [ fuzz formattedText "works the same as String.trimLeft" <|
                \formatted ->
                    FT.trimLeft formatted
                        |> FT.text
                        |> Expect.equal (FT.text formatted |> String.trimLeft)
            ]
        , describe ".trimRight"
            [ fuzz formattedText "works the same as String.trimRight" <|
                \formatted ->
                    FT.trimRight formatted
                        |> FT.text
                        |> Expect.equal (FT.text formatted |> String.trimRight)
            ]
        , describe ".indexes"
            [ fuzz2 repetitiveStringElement repetitiveString "works the same as String.indexes" <|
                \part whole ->
                    FT.indexes (FT.fromString part) (FT.fromString whole)
                        |> Expect.equal (String.indexes part whole)
            , fuzz2 repetitiveFText repetitiveFTextElement "only shows indexes when formats match too" <|
                \part whole ->
                    let
                        stringIndexes : List Int
                        stringIndexes =
                            String.indexes (FT.text part) (FT.text whole)

                        formattedIndexes : List Int
                        formattedIndexes =
                            FT.indexes part whole
                    in
                    stringIndexes
                        |> assertForAll
                            (\index ->
                                if List.member index formattedIndexes then
                                    assertPartAt part whole index
                                else
                                    assertPartNotAt part whole index
                            )
            ]
        , describe ".contains"
            [ fuzz2 repetitiveStringElement repetitiveString "works the same as String.contains" <|
                \part whole ->
                    FT.contains (FT.fromString part) (FT.fromString whole)
                        |> Expect.equal (String.contains part whole)
            , fuzz2 repetitiveFText repetitiveFTextElement "returns false if markup does not match" <|
                \part whole ->
                    let
                        stringIndexes =
                            String.indexes (FT.text part) (FT.text whole)
                    in
                    if FT.contains part whole then
                        -- This branch where we have a match is hard to test because we'd need an assertAny.
                        -- It's also already covered by the previous test.
                        Expect.pass
                    else
                        stringIndexes
                            |> assertForAll (\index -> assertPartNotAt part whole index)
            ]
        , describe ".startsWith"
            [ fuzz2 repetitiveStringElement repetitiveString "works the same as String.startsWith" <|
                \start whole ->
                    FT.startsWith (FT.fromString start) (FT.fromString whole)
                        |> Expect.equal (String.startsWith start whole)
            , fuzz2 formattedText formattedText "returns true if the markup matches" <|
                \start end ->
                    let
                        whole : FormattedText Markup
                        whole =
                            FT.append start end
                    in
                    FT.startsWith start whole
                        |> Expect.true "Expected startsWith to return true"
            , fuzz2 formattedText formattedText "returns false if markup does not match" <|
                \start end ->
                    let
                        modifiedStart : FormattedText Markup
                        modifiedStart =
                            FT.addRange { tag = Yellow, start = 0, end = 1 } start

                        whole : FormattedText Markup
                        whole =
                            FT.append start end
                    in
                    if FT.isEmpty start then
                        Expect.pass
                    else
                        FT.startsWith modifiedStart whole
                            |> Expect.false "Expected startsWith to return false"
            ]
        , describe ".endsWith"
            [ fuzz2 repetitiveStringElement repetitiveString "works the same as String.endsWith" <|
                \end whole ->
                    FT.endsWith (FT.fromString end) (FT.fromString whole)
                        |> Expect.equal (String.endsWith end whole)
            , fuzz2 formattedText formattedText "returns true if the markup matches" <|
                \start end ->
                    let
                        whole : FormattedText Markup
                        whole =
                            FT.append start end
                    in
                    FT.endsWith end whole
                        |> Expect.true "Expected endsWith to return true"
            , fuzz2 formattedText formattedText "returns false if markup does not match" <|
                \start end ->
                    let
                        modifiedStart : FormattedText Markup
                        modifiedStart =
                            FT.addRange { tag = Yellow, start = 0, end = 1 } end

                        whole : FormattedText Markup
                        whole =
                            FT.append start end
                    in
                    if FT.isEmpty end then
                        Expect.pass
                    else
                        FT.endsWith modifiedStart whole
                            |> Expect.false "Expected endsWith to return false"
            ]
        , describe ".toInt"
            [ fuzz formattedText "works like String.toInt" <|
                \formatted ->
                    FT.toInt formatted
                        |> (\result ->
                                -- Weirdly we can get 'Ok NaN' here, for instance for the string "+".
                                -- We can't use isNaN, because elm-make will only allow use of that on floats.
                                -- This result is expected to contain an Int.
                                if toString result == "Ok NaN" then
                                    Expect.pass
                                else
                                    result |> Expect.equal (FT.text formatted |> String.toInt)
                           )
            ]
        , describe ".toFloat"
            [ fuzz formattedText "works like String.toFloat" <|
                \formatted ->
                    FT.toFloat formatted
                        |> Expect.equal (FT.text formatted |> String.toFloat)
            ]
        , describe ".toList"
            [ fuzz formattedText "works like String.toList" <|
                \formatted ->
                    FT.toList formatted
                        |> Expect.equal (FT.text formatted |> String.toList)
            ]
        , describe ".fromList"
            [ fuzz (list char) "works like String.fromList" <|
                \chars ->
                    FT.fromList chars
                        |> Expect.equal (String.fromList chars |> FT.fromString)
            ]
        , describe ".toUpper"
            [ fuzz formattedText "works like String.toUpper" <|
                \formatted ->
                    FT.toUpper formatted
                        |> FT.text
                        |> Expect.equal (FT.text formatted |> String.toUpper)
            ]
        , describe ".toLower"
            [ fuzz formattedText "works like String.toLower" <|
                \formatted ->
                    FT.toLower formatted
                        |> FT.text
                        |> Expect.equal (FT.text formatted |> String.toLower)
            ]
        , describe ".padLeft"
            [ fuzz3 char (intRange -10 1000) formattedText "works like String.padLeft" <|
                \char upTo formatted ->
                    FT.padLeft upTo char [] formatted
                        |> FT.text
                        |> Expect.equal (FT.text formatted |> String.padLeft upTo char)
            , fuzz3 char (intRange -10 1000) formattedText "Adds the right markup to the padding" <|
                \char upTo formatted ->
                    let
                        paddingLength : Int
                        paddingLength =
                            upTo - FT.length formatted

                        padding : FormattedText Markup
                        padding =
                            FT.fromChar char
                                |> FT.formatAll Yellow
                                |> FT.repeat paddingLength
                    in
                    FT.padLeft upTo char [ Yellow ] formatted
                        |> FT.left paddingLength
                        |> equalFormattedTexts padding
            ]
        , describe ".padRight"
            [ fuzz3 char (intRange -10 1000) formattedText "works like String.padRight" <|
                \char upTo formatted ->
                    FT.padRight upTo char [] formatted
                        |> FT.text
                        |> Expect.equal (FT.text formatted |> String.padRight upTo char)
            , fuzz3 char (intRange -10 1000) formattedText "Adds the right markup to the padding" <|
                \char upTo formatted ->
                    let
                        paddingLength : Int
                        paddingLength =
                            upTo - FT.length formatted

                        padding : FormattedText Markup
                        padding =
                            FT.fromChar char
                                |> FT.formatAll Yellow
                                |> FT.repeat paddingLength
                    in
                    FT.padRight upTo char [ Yellow ] formatted
                        |> FT.right paddingLength
                        |> equalFormattedTexts padding
            ]
        , describe ".pad"
            [ fuzz3 char (intRange -10 1000) formattedText "works like String.pad" <|
                \char upTo formatted ->
                    FT.pad upTo char [] formatted
                        |> FT.text
                        |> Expect.equal (FT.text formatted |> String.pad upTo char)
            ]
        ]


repetitiveString : Fuzzer String
repetitiveString =
    Fuzz.list repetitiveStringElement
        |> Fuzz.map String.concat


repetitiveStringElement : Fuzzer String
repetitiveStringElement =
    Fuzz.oneOf
        [ Fuzz.constant "a"
        , Fuzz.constant "bb"
        , Fuzz.constant "ccc"
        , string
        ]


repetitiveFText : Fuzzer (FormattedText Markup)
repetitiveFText =
    customFormattedText repetitiveStringElement markup


repetitiveFTextElement : Fuzzer (FormattedText Markup)
repetitiveFTextElement =
    customFormattedText repetitiveString markup


assertPartAt : FormattedText Markup -> FormattedText Markup -> Int -> Expectation
assertPartAt part whole index =
    FT.slice index (index + FT.length part) whole
        |> Expect.equal part


assertPartNotAt : FormattedText Markup -> FormattedText Markup -> Int -> Expectation
assertPartNotAt part whole index =
    FT.slice index (index + FT.length part) whole
        |> Expect.notEqual part
