module Spec.FormattedText.Markdown exposing (parseThenView)

import Expect
import FormattedText.Markdown as Markdown
import Html exposing (a, code, em, h1, p, strong, text)
import Html.Attributes exposing (href)
import Test exposing (..)


markdown : String
markdown =
    """
# A header

This **amazing** _string_ contains [markdown](https://en.wikipedia.org/wiki/Markdown) `code`
"""


parseThenView : Test
parseThenView =
    test "parse then view markdown" <|
        \_ ->
            markdown
                |> Markdown.parse
                |> Markdown.view
                |> Expect.equal
                    [ h1 [] [ text "A header" ]
                    , p []
                        [ text "This "
                        , strong [] [ text "amazing" ]
                        , text " "
                        , em [] [ text "string" ]
                        , text " contains "
                        , a [ href "https://en.wikipedia.org/wiki/Markdown" ] [ text "markdown" ]
                        , text " "
                        , code [] [ text "code" ]
                        ]
                    ]
