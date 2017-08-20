module Spec.FormattedText.Regex exposing (spec)

import Expect exposing (Expectation)
import FormattedText exposing (FormattedText, Range)
import FormattedText.Regex
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Regex
import Test exposing (..)
import Util exposing (assertForAll, equalFormattedTexts, formattedText)


spec : Test
spec =
    describe "FormattedText.Regex"
        [ describe ".find"
            [ fuzz2 howMany formattedText "works the same way as Regex.find" <|
                \howMany formatted ->
                    let
                        regex : Regex.Regex
                        regex =
                            Regex.regex "[a-z]+"

                        fromStringMatch : Regex.Match -> ( String, Int, Int )
                        fromStringMatch { match, index, number } =
                            ( match, index, number )

                        fromFormattedMatch : FormattedText.Regex.Match markup -> ( String, Int, Int )
                        fromFormattedMatch { match, index, number } =
                            ( FormattedText.text match, index, number )
                    in
                    FormattedText.Regex.find howMany regex formatted
                        |> List.map fromFormattedMatch
                        |> Expect.equal
                            (FormattedText.text formatted
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
                    FormattedText.Regex.find howMany regex formatted
                        |> assertForAll
                            (\{ match, index } ->
                                FormattedText.slice index (index + FormattedText.length match) formatted
                                    |> Expect.equal match
                            )
            ]
        , describe ".replace"
            [ fuzz3 howMany formattedText replacer "works the same as Regex.replace" <|
                \howMany formatted ( _, replacer ) ->
                    let
                        regex : Regex.Regex
                        regex =
                            Regex.regex "[a-z]+"

                        foo : String -> String
                        foo =
                            replacer

                        formattedReplacer : FormattedText.Regex.Match markup -> FormattedText markup
                        formattedReplacer { match } =
                            match
                                |> FormattedText.text
                                |> replacer
                                |> FormattedText.fromString
                    in
                    FormattedText.Regex.replace howMany regex formattedReplacer formatted
                        |> FormattedText.text
                        |> Expect.equal
                            (Regex.replace
                                howMany
                                regex
                                (.match >> replacer)
                                (FormattedText.text formatted)
                            )
            , fuzz2 howMany formattedText "Replacing with identity gives back original result" <|
                \howMany formatted ->
                    let
                        regex : Regex.Regex
                        regex =
                            Regex.regex "[a-z]+"
                    in
                    FormattedText.Regex.replace howMany regex (.match >> identity) formatted
                        |> equalFormattedTexts formatted
            ]
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
