module Spec.FormattedText.Markdown exposing (parseThenView)

import Expect
import FormattedText.Markdown as Markdown
import Html exposing (a, code, em, strong, text)
import Html.Attributes exposing (href)
import Test exposing (..)


parseThenView : Test
parseThenView =
    test "parse then view markdown" <|
        \_ ->
            "This **amazing** _string_ contains [markdown](https://en.wikipedia.org/wiki/Markdown) `code`"
                |> Markdown.parse
                |> Result.map Markdown.view
                |> Expect.equal
                    (Ok
                        [ text "This "
                        , strong [] [ text "amazing" ]
                        , text " "
                        , em [] [ text "string" ]
                        , text " contains "
                        , a [ href "https://en.wikipedia.org/wiki/Markdown" ] [ text "markdown" ]
                        , text " "
                        , code [] [ text "code" ]
                        ]
                    )
