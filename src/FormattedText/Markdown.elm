module FormattedText.Markdown exposing (Markdown(..), parse, view)

{-| A specific FormattedText type for inline markdown.

This intentionally does not support block-level markdown elements,
as FormattedText is not suitable for that.
If you have formatted inline strings embeded in a semantic structure
you might want to create a custom type for that structure.

@docs Markdown
@docs parse
@docs view

-}

import FormattedText as FT exposing (FormattedText)
import Html exposing (Html)
import Html.Attributes as Attr
import Markdown.Block
import Markdown.Config
import Markdown.Inline


type Markdown
    = Code
    | Link String
    | Bold
    | Italic


parse : String -> Result String (FormattedText Markdown)
parse markdown =
    let
        options : Markdown.Config.Options
        options =
            { softAsHardLineBreak = False
            , rawHtml = Markdown.Config.DontParse
            }
    in
    case Markdown.Block.parse (Just options) markdown of
        [ Markdown.Block.PlainInlines inlines ] ->
            Ok (List.map fromInline inlines |> FT.concat)

        [ Markdown.Block.Paragraph _ inlines ] ->
            Ok (List.map fromInline inlines |> FT.concat)

        _ ->
            Err "Block level markdown elements are not supported."


view : FormattedText Markdown -> List (Html msg)
view formatted =
    FT.chunks viewChunk formatted


viewChunk : String -> List Markdown -> Html msg
viewChunk text tags =
    viewTag tags (Html.text text)


viewTag : List Markdown -> Html msg -> Html msg
viewTag tags child =
    case tags of
        [] ->
            child

        Bold :: xs ->
            Html.em [] [ viewTag xs child ]

        Italic :: xs ->
            Html.em [] [ viewTag xs child ]

        Code :: xs ->
            Html.code [] [ viewTag xs child ]

        (Link link) :: xs ->
            Html.a
                [ Attr.href link ]
                [ viewTag xs child ]


fromInline : Markdown.Inline.Inline i -> FormattedText Markdown
fromInline inline =
    case inline of
        Markdown.Inline.Text string ->
            FT.fromString string

        Markdown.Inline.HardLineBreak ->
            FT.fromChar '\n'

        Markdown.Inline.CodeInline string ->
            FT.fromString string |> FT.formatAll Code

        Markdown.Inline.Link link _ subInlines ->
            List.map fromInline subInlines
                |> FT.concat
                |> FT.formatAll (Link link)

        Markdown.Inline.Image link _ subInlines ->
            -- An image is not really an inline element.
            -- We're just going to render it as a link.
            List.map fromInline subInlines
                |> FT.concat
                |> FT.formatAll (Link link)

        Markdown.Inline.HtmlInline _ _ _ ->
            -- We're not parsing Html so we should never get this.
            FT.empty

        Markdown.Inline.Emphasis 0 subInlines ->
            List.map fromInline subInlines
                |> FT.concat

        Markdown.Inline.Emphasis 1 subInlines ->
            List.map fromInline subInlines
                |> FT.concat
                |> FT.formatAll Italic

        Markdown.Inline.Emphasis 2 subInlines ->
            List.map fromInline subInlines
                |> FT.concat
                |> FT.formatAll Bold

        Markdown.Inline.Emphasis _ subInlines ->
            List.map fromInline subInlines
                |> FT.concat
                |> FT.formatAll Bold
                |> FT.formatAll Italic

        Markdown.Inline.Custom _ _ ->
            -- We're not defining custom elements so we should never get this.
            FT.empty
