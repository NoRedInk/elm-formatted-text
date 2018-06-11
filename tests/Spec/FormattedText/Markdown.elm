module Spec.FormattedText.Markdown
    exposing
        ( parseInlineThenViewInline
        , parseList
        , parseThenView
        )

import Expect
import FormattedText
import FormattedText.Markdown as Markdown exposing (..)
import Html exposing (a, code, em, h1, li, p, strong, text, ul)
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


markdownList : String
markdownList =
    """* this
  * is nested"""



-- * is nested


parseList : Test
parseList =
    test "parse list" <|
        \_ ->
            markdownList
                |> Markdown.parse
                |> Expect.equal
                    [ UnOrderedList
                        [ [ PlainInline
                                (FormattedText.fromString "this")
                          ]
                        , [ UnOrderedList
                                [ [ PlainInline
                                        (FormattedText.fromString "is nested")
                                  ]
                                ]
                          ]
                        ]
                    ]


parseInlineThenViewInline : Test
parseInlineThenViewInline =
    describe "parse inline markdown"
        [ test "newlines at the end/start shouldn't matter" <|
            \() ->
                "This is **cool**!"
                    |> Markdown.parseInline
                    |> Result.map Markdown.viewInline
                    |> Expect.equal
                        (Ok
                            [ text "This is "
                            , strong [] [ text "cool" ]
                            , text "!"
                            ]
                        )
        ]
