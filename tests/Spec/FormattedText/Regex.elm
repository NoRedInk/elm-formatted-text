module Spec.FormattedText.Regex exposing (..)

import Basics.Extra exposing (..)
import Expect exposing (Expectation)
import FormattedText as FT exposing (FormattedText, Range)
import FormattedText.Regex as FTRegex
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Regex
import Spec.FormattedText.Fuzz exposing (Markup, equals, formattedText)
import Test exposing (..)
import Util exposing (assertForAll)


find : Test
find =
    describe ".find"
        [ fuzz formattedText "works the same way as Regex.find" <|
            \formatted ->
                let
                    regex : Regex.Regex
                    regex =
                        Regex.fromString "[a-z]+"
                            |> Maybe.withDefault Regex.never

                    fromStringMatch : Regex.Match -> ( String, Int, Int )
                    fromStringMatch { match, index, number } =
                        ( match, index, number )

                    fromFormattedMatch : FTRegex.Match markup -> ( String, Int, Int )
                    fromFormattedMatch { match, index, number } =
                        ( FT.text match, index, number )
                in
                FTRegex.find regex formatted
                    |> List.map fromFormattedMatch
                    |> Expect.equal
                        (FT.text formatted
                            |> Regex.find regex
                            |> List.map fromStringMatch
                        )
        , fuzz formattedText "matches are sub-texts of full text" <|
            \formatted ->
                let
                    regex : Regex.Regex
                    regex =
                        Regex.fromString "[a-z]+"
                            |> Maybe.withDefault Regex.never
                in
                FTRegex.find regex formatted
                    |> assertForAll
                        (\{ match, index } ->
                            FT.slice index (index + FT.length match) formatted
                                |> Expect.equal match
                        )
        ]


replace : Test
replace =
    describe ".replace"
        [ fuzz2 formattedText replacer "works the same as Regex.replace" <|
            \formatted ( _, replacer_ ) ->
                let
                    regex : Regex.Regex
                    regex =
                        Regex.fromString "[a-z]+"
                            |> Maybe.withDefault Regex.never

                    foo : String -> String
                    foo =
                        replacer_

                    formattedReplacer : FTRegex.Match markup -> FormattedText markup
                    formattedReplacer { match } =
                        match
                            |> FT.text
                            |> replacer_
                            |> FT.fromString
                in
                FTRegex.replace regex formattedReplacer formatted
                    |> FT.text
                    |> Expect.equal
                        (Regex.replace
                            regex
                            (.match >> replacer_)
                            (FT.text formatted)
                        )
        , fuzz formattedText "Replacing with identity gives back original result" <|
            \formatted ->
                let
                    regex : Regex.Regex
                    regex =
                        Regex.fromString "[a-z]+"
                            |> Maybe.withDefault Regex.never
                in
                FTRegex.replace regex (.match >> identity) formatted
                    |> equals formatted
        ]


split : Test
split =
    describe ".split"
        [ fuzz formattedText "works the same as Regex.split" <|
            \formatted ->
                let
                    regex : Regex.Regex
                    regex =
                        Regex.fromString "[a-z]+"
                            |> Maybe.withDefault Regex.never
                in
                FTRegex.split regex formatted
                    |> List.map FT.text
                    |> Expect.equal (FT.text formatted |> Regex.split regex)
        , fuzz formattedText "results from .find and .split constructor original input" <|
            \formatted ->
                let
                    regex : Regex.Regex
                    regex =
                        Regex.fromString "[a-z]+"
                            |> Maybe.withDefault Regex.never

                    matches : List (FormattedText Markup)
                    matches =
                        FTRegex.find regex formatted
                            |> List.map .match

                    nonMatches : List (FormattedText Markup)
                    nonMatches =
                        FTRegex.split regex formatted
                in
                interweave nonMatches matches
                    |> FT.concat
                    |> equals formatted
        ]


contains : Test
contains =
    describe ".contains"
        [ fuzz formattedText "works the same as Regex.contains" <|
            \formatted ->
                let
                    regex : Regex.Regex
                    regex =
                        Regex.fromString "[a-z]+"
                            |> Maybe.withDefault Regex.never
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


{-| Create a new list by taking turns taking an element from two lists, starting with the first list.
-}
interweave : List a -> List a -> List a
interweave xs ys =
    List.map2 (\x y -> [ x, y ]) xs ys
        |> List.concat
        -- If either list is longer than the other, append its remainder.
        |> flip List.append (List.drop (List.length xs) ys)
        |> flip List.append (List.drop (List.length ys) xs)
