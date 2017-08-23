module Spec.FormattedText exposing (..)

import Char
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


noOverlap : Test
noOverlap =
    fuzz formattedText "Formatted text ranges with equal markup never overlap" <|
        \formatted ->
            FT.ranges formatted
                |> Dict.Extra.groupBy (.tag >> toString)
                |> Dict.values
                |> assertForAll rangesDontOverlap


noMerge : Test
noMerge =
    test "Ranges that do not overlap are not merged into one" <|
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


append : Test
append =
    fuzz2 string string ".append" <|
        \a b ->
            FT.append (FT.fromString a) (FT.fromString b)
                |> equalFormattedTexts (FT.fromString <| a ++ b)


concat : Test
concat =
    fuzz (list string) ".concat" <|
        \xs ->
            List.map FT.fromString xs
                |> FT.concat
                |> equalFormattedTexts (FT.fromString <| String.concat xs)


fromStringTextDuality : Test
fromStringTextDuality =
    fuzz string "fromString and text are duals" <|
        \text ->
            FT.fromString text
                |> FT.text
                |> Expect.equal text


chunks : Test
chunks =
    test ".chunks" <|
        \_ ->
            FT.formattedText
                "foo bar baz"
                [ { tag = "a", start = 0, end = 3 }, { tag = "b", start = 8, end = 11 } ]
                |> FT.chunks (,)
                |> Expect.equal [ ( "foo", [ "a" ] ), ( " bar ", [] ), ( "baz", [ "b" ] ) ]


chunksUnchunkDuality : Test
chunksUnchunkDuality =
    fuzz formattedText "chunks and unchunk are duals" <|
        \formatted ->
            formatted
                |> FT.chunks (,)
                |> FT.unchunk
                |> equalFormattedTexts formatted


reverse : Test
reverse =
    describe ".reverse"
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


repeat : Test
repeat =
    fuzz2 formattedText (intRange 0 5) ".repeat" <|
        \formatted n ->
            formatted
                |> FT.repeat n
                |> FT.text
                |> Expect.equal (formatted |> FT.text |> String.repeat n)


cons : Test
cons =
    describe ".cons"
        [ fuzz formattedText "works the same as String.cons" <|
            \formatted ->
                formatted
                    |> FT.cons 'a'
                    |> FT.text
                    |> Expect.equal (formatted |> FT.text |> String.cons 'a')
        ]


uncons : Test
uncons =
    describe ".uncons"
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


left : Test
left =
    describe ".left"
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


right : Test
right =
    describe ".right"
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


dropLeft : Test
dropLeft =
    describe ".dropLeft"
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


dropRight : Test
dropRight =
    describe ".dropRight"
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


slice : Test
slice =
    describe ".slice"
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


split : Test
split =
    describe ".split"
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


join : Test
join =
    describe ".join"
        [ fuzz2 formattedText (shortList formattedText) "works the same as String.join" <|
            \joiner parts ->
                FT.join joiner parts
                    |> FT.text
                    |> Expect.equal (String.join (FT.text joiner) (List.map FT.text parts))
        ]


joinSplitDuality : Test
joinSplitDuality =
    fuzz (atLeastOneList formattedText) ".join and .split are duals" <|
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


lines : Test
lines =
    describe ".lines"
        [ fuzz formattedText "works the same as String.lines" <|
            \formatted ->
                FT.lines formatted
                    |> List.map FT.text
                    |> Expect.equal (FT.text formatted |> String.lines)
        ]


words : Test
words =
    describe ".words"
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


trim : Test
trim =
    describe ".trim"
        [ fuzz formattedText "works the same as String.trim" <|
            \formatted ->
                FT.trim formatted
                    |> FT.text
                    |> Expect.equal (FT.text formatted |> String.trim)
        ]


trimLeft : Test
trimLeft =
    describe ".trimLeft"
        [ fuzz formattedText "works the same as String.trimLeft" <|
            \formatted ->
                FT.trimLeft formatted
                    |> FT.text
                    |> Expect.equal (FT.text formatted |> String.trimLeft)
        ]


trimRight : Test
trimRight =
    describe ".trimRight"
        [ fuzz formattedText "works the same as String.trimRight" <|
            \formatted ->
                FT.trimRight formatted
                    |> FT.text
                    |> Expect.equal (FT.text formatted |> String.trimRight)
        ]


indexes : Test
indexes =
    describe ".indexes"
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


contains : Test
contains =
    describe ".contains"
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


startsWith : Test
startsWith =
    describe ".startsWith"
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


endsWith : Test
endsWith =
    describe ".endsWith"
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


toInt : Test
toInt =
    describe ".toInt"
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


toFloat : Test
toFloat =
    describe ".toFloat"
        [ fuzz formattedText "works like String.toFloat" <|
            \formatted ->
                FT.toFloat formatted
                    |> Expect.equal (FT.text formatted |> String.toFloat)
        ]


toList : Test
toList =
    describe ".toList"
        [ fuzz formattedText "works like String.toList" <|
            \formatted ->
                FT.toList formatted
                    |> Expect.equal (FT.text formatted |> String.toList)
        ]


fromList : Test
fromList =
    describe ".fromList"
        [ fuzz (list char) "works like String.fromList" <|
            \chars ->
                FT.fromList chars
                    |> Expect.equal (String.fromList chars |> FT.fromString)
        ]


toUpper : Test
toUpper =
    describe ".toUpper"
        [ fuzz formattedText "works like String.toUpper" <|
            \formatted ->
                FT.toUpper formatted
                    |> FT.text
                    |> Expect.equal (FT.text formatted |> String.toUpper)
        ]


toLower : Test
toLower =
    describe ".toLower"
        [ fuzz formattedText "works like String.toLower" <|
            \formatted ->
                FT.toLower formatted
                    |> FT.text
                    |> Expect.equal (FT.text formatted |> String.toLower)
        ]


padLeft : Test
padLeft =
    describe ".padLeft"
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


padRight : Test
padRight =
    describe ".padRight"
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


pad : Test
pad =
    describe ".pad"
        [ fuzz3 char (intRange -10 1000) formattedText "works like String.pad" <|
            \char upTo formatted ->
                FT.pad upTo char [] formatted
                    |> FT.text
                    |> Expect.equal (FT.text formatted |> String.pad upTo char)
        ]


map : Test
map =
    describe ".map"
        [ fuzz formattedText "works like String.map" <|
            \formatted ->
                FT.map Char.toUpper formatted
                    |> FT.text
                    |> Expect.equal (FT.text formatted |> String.map Char.toUpper)
        , fuzz formattedText "does not touch formatting" <|
            \formatted ->
                FT.map identity formatted
                    |> equalFormattedTexts formatted
        ]


foldl : Test
foldl =
    describe ".foldl"
        [ fuzz formattedText "works like String.foldl" <|
            \formatted ->
                FT.foldl String.cons "" formatted
                    |> Expect.equal (FT.text formatted |> String.foldl String.cons "")
        ]


foldr : Test
foldr =
    describe ".foldr"
        [ fuzz formattedText "works like String.foldr" <|
            \formatted ->
                FT.foldr String.cons "" formatted
                    |> Expect.equal (FT.text formatted |> String.foldr String.cons "")
        ]


filter : Test
filter =
    describe ".filter"
        [ fuzz formattedText "works like String.filter" <|
            \formatted ->
                FT.filter Char.isUpper formatted
                    |> FT.text
                    |> Expect.equal (FT.text formatted |> String.filter Char.isUpper)
        , fuzz formattedText "does not touch formatting" <|
            \formatted ->
                FT.filter Char.isUpper formatted
                    |> equalFormattedTexts
                        (FTRegex.replace
                            Regex.All
                            (Regex.regex "[^A-Z]+")
                            (always FT.empty)
                            formatted
                        )
        ]


any : Test
any =
    describe ".any"
        [ fuzz formattedText "works like String.any" <|
            \formatted ->
                FT.any Char.isUpper formatted
                    |> Expect.equal (FT.text formatted |> String.any Char.isUpper)
        ]


all : Test
all =
    describe ".all"
        [ fuzz formattedText "works like String.all" <|
            \formatted ->
                FT.all Char.isUpper formatted
                    |> Expect.equal (FT.text formatted |> String.all Char.isUpper)
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
