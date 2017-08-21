module Spec.FormattedText.Regex exposing (..)

import Expect exposing (Expectation)
import FormattedText as FT exposing (FormattedText, Range)
import FormattedText.Fuzz exposing (Markup, formattedText)
import FormattedText.Regex as FTRegex
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Regex
import Test exposing (..)
import Util exposing (assertForAll, equalFormattedTexts)


find : Test
find =
    describe ".find"
        [ fuzz2 howMany formattedText "works the same way as Regex.find" <|
            \howMany formatted ->
                let
                    regex : Regex.Regex
                    regex =
                        Regex.regex "[a-z]+"

                    fromStringMatch : Regex.Match -> ( String, Int, Int )
                    fromStringMatch { match, index, number } =
                        ( match, index, number )

                    fromFormattedMatch : FTRegex.Match markup -> ( String, Int, Int )
                    fromFormattedMatch { match, index, number } =
                        ( FT.text match, index, number )
                in
                FTRegex.find howMany regex formatted
                    |> List.map fromFormattedMatch
                    |> Expect.equal
                        (FT.text formatted
                            |> Regex.find howMany regex
                            |> List.map fromStringMatch
                        )
        , fuzz2 howMany formattedText "matches are sub-texts of full text" <|
            \howMany formatted ->
                let
                    regex : Regex.Regex
                    regex =
                        Regex.regex "[a-z]+"
                in
                FTRegex.find howMany regex formatted
                    |> assertForAll
                        (\{ match, index } ->
                            FT.slice index (index + FT.length match) formatted
                                |> Expect.equal match
                        )
        ]


replace : Test
replace =
    describe ".replace"
        [ fuzz3 howMany formattedText replacer "works the same as Regex.replace" <|
            \howMany formatted ( _, replacer ) ->
                let
                    regex : Regex.Regex
                    regex =
                        Regex.regex "[a-z]+"

                    foo : String -> String
                    foo =
                        replacer

                    formattedReplacer : FTRegex.Match markup -> FormattedText markup
                    formattedReplacer { match } =
                        match
                            |> FT.text
                            |> replacer
                            |> FT.fromString
                in
                FTRegex.replace howMany regex formattedReplacer formatted
                    |> FT.text
                    |> Expect.equal
                        (Regex.replace
                            howMany
                            regex
                            (.match >> replacer)
                            (FT.text formatted)
                        )
        , fuzz2 howMany formattedText "Replacing with identity gives back original result" <|
            \howMany formatted ->
                let
                    regex : Regex.Regex
                    regex =
                        Regex.regex "[a-z]+"
                in
                FTRegex.replace howMany regex (.match >> identity) formatted
                    |> equalFormattedTexts formatted
        ]


split : Test
split =
    describe ".split"
        [ fuzz2 howMany formattedText "works the same as Regex.split" <|
            \howMany formatted ->
                let
                    regex : Regex.Regex
                    regex =
                        Regex.regex "[a-z]+"
                in
                FTRegex.split howMany regex formatted
                    |> List.map FT.text
                    |> Expect.equal (FT.text formatted |> Regex.split howMany regex)
        , fuzz2 howMany formattedText "results from .find and .split constructor original input" <|
            \howMany formatted ->
                let
                    regex : Regex.Regex
                    regex =
                        Regex.regex "[a-z]+"

                    -- `Regex.split` treats negative `AtMost` values as `All`, `Regex.find` treats them as `AtMost 0.
                    -- Because we want to implement the `split behaviour` using `find` we need to transform.
                    fixedHowMany : Regex.HowMany
                    fixedHowMany =
                        case howMany of
                            Regex.All ->
                                howMany

                            Regex.AtMost n ->
                                if n < 0 then
                                    Regex.All
                                else
                                    howMany

                    matches : List (FormattedText Markup)
                    matches =
                        FTRegex.find fixedHowMany regex formatted
                            |> List.map .match

                    nonMatches : List (FormattedText Markup)
                    nonMatches =
                        FTRegex.split fixedHowMany regex formatted
                in
                interweave nonMatches matches
                    |> FT.concat
                    |> equalFormattedTexts formatted
        ]


contains : Test
contains =
    describe ".contains"
        [ fuzz formattedText "works the same as Regex.contains" <|
            \formatted ->
                let
                    regex : Regex.Regex
                    regex =
                        Regex.regex "[a-z]+"
                in
                FTRegex.contains regex formatted
                    |> Expect.equal (FT.text formatted |> Regex.contains regex)
        ]


{-| A fuzzed function in a test failure will be displayed as "<function>".
That's not useful, so we add a string representation of the function for debugging.
-}
replacer : Fuzzer ( String, String -> String )
replacer =
    Fuzz.oneOf
        [ Fuzz.constant ( "identity", identity )

        -- Keep fragment length the same.
        , Fuzz.constant ( "String.toUpper", String.toUpper )

        -- Make fragment shorter.
        , Fuzz.constant ( "String.left 2", String.left 2 )

        -- Make fragment longer.
        , Fuzz.constant ( "(++) \"!!\"", (++) "!!" )
        ]


howMany : Fuzzer Regex.HowMany
howMany =
    Fuzz.oneOf
        [ Fuzz.constant Regex.All
        , Fuzz.map Regex.AtMost Fuzz.int
        ]


{-| Create a new list by taking turns taking an element from two lists, starting with the first list.
-}
interweave : List a -> List a -> List a
interweave xs ys =
    List.map2 (\x y -> [ x, y ]) xs ys
        |> List.concat
        -- If either list is longer than the other, append its remainder.
        |> flip List.append (List.drop (List.length xs) ys)
        |> flip List.append (List.drop (List.length ys) xs)
