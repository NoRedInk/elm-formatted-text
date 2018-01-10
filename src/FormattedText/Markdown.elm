module FormattedText.Markdown exposing (Block(..), Markdown(..), parse, view, viewInline)

{-| A specific FormattedText type for inline markdown.

This intentionally does not support block-level markdown elements,
as FormattedText is not suitable for that.
If you have formatted inline strings embeded in a semantic structure
you might want to create a custom type for that structure.

@docs Markdown
@docs Block
@docs parse
@docs view
@docs viewInline

-}

import FormattedText as FT exposing (FormattedText)
import Html exposing (Html)
import Html.Attributes as Attr
import Markdown.Block
import Markdown.Config
import Markdown.Inline


{-| The different types of inline formatting that Markdown supports.
-}
type Markdown
    = Code
    | Link String
    | Bold
    | Italic


{-| The types of block formatting Markdown supports.
-}
type Block
    = ThematicBreak
    | Heading Int (FormattedText Markdown)
    | CodeBlock String
    | Paragraph (FormattedText Markdown)
    | BlockQuote (List Block)
    | UnOrderedList (List (List Block))
    | OrderedList (List (List Block))
    | PlainInline (FormattedText Markdown)


{-| Turn a markdown-formatted string into Blocks.

The inline portions of the Block structure will be instances of FormattedText.

-}
parse : String -> List Block
parse markdown =
    let
        options : Markdown.Config.Options
        options =
            { softAsHardLineBreak = False
            , rawHtml = Markdown.Config.DontParse
            }
    in
    Markdown.Block.parse (Just options) markdown
        |> List.concatMap parseBlock


{-| Render the markdown-formatted text as Html.
-}
view : List Block -> List (Html msg)
view blocks =
    List.concatMap viewBlock blocks


viewBlock : Block -> List (Html msg)
viewBlock block =
    case block of
        ThematicBreak ->
            [ Html.hr [] []
            ]

        Heading 1 formatted ->
            Html.h1 [] (viewInline formatted)
                |> List.singleton

        Heading 2 formatted ->
            Html.h2 [] (viewInline formatted)
                |> List.singleton

        Heading 3 formatted ->
            Html.h3 [] (viewInline formatted)
                |> List.singleton

        Heading 4 formatted ->
            Html.h4 [] (viewInline formatted)
                |> List.singleton

        Heading 5 formatted ->
            Html.h5 [] (viewInline formatted)
                |> List.singleton

        Heading _ formatted ->
            Html.h6 [] (viewInline formatted)
                |> List.singleton

        CodeBlock contents ->
            Html.pre [] [ Html.text contents ]
                |> List.singleton

        Paragraph formatted ->
            Html.p [] (viewInline formatted)
                |> List.singleton

        BlockQuote blocks ->
            List.concatMap viewBlock blocks
                |> Html.blockquote []
                |> List.singleton

        UnOrderedList items ->
            List.map viewItem items
                |> Html.ul []
                |> List.singleton

        OrderedList items ->
            List.map viewItem items
                |> Html.ol []
                |> List.singleton

        PlainInline formatted ->
            viewInline formatted


viewItem : List Block -> Html msg
viewItem blocks =
    List.concatMap viewBlock blocks
        |> Html.li []


{-| If you have your own logic for rendering the markdown block elements, you
can call this function to render the inline portions of the Markdown.

If you want to render your markdown in a different way take a look at the implementation of this function
to see how you can use `FormattedText.chunks` to do so in a simple way.

-}
viewInline : FormattedText Markdown -> List (Html msg)
viewInline formatted =
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
            Html.strong [] [ viewTag xs child ]

        Italic :: xs ->
            Html.em [] [ viewTag xs child ]

        Code :: xs ->
            Html.code [] [ viewTag xs child ]

        (Link link) :: xs ->
            Html.a
                [ Attr.href link ]
                [ viewTag xs child ]


parseBlock : Markdown.Block.Block b i -> List Block
parseBlock block =
    case block of
        Markdown.Block.BlankLine contents ->
            FT.fromString contents
                |> PlainInline
                |> List.singleton

        Markdown.Block.ThematicBreak ->
            [ ThematicBreak ]

        Markdown.Block.Heading _ level inlines ->
            Heading level (List.map parseInline inlines |> FT.concat)
                |> List.singleton

        Markdown.Block.CodeBlock _ contents ->
            [ CodeBlock contents ]

        Markdown.Block.Paragraph _ inlines ->
            Paragraph (List.map parseInline inlines |> FT.concat)
                |> List.singleton

        Markdown.Block.BlockQuote blocks ->
            List.concatMap parseBlock blocks
                |> BlockQuote
                |> List.singleton

        Markdown.Block.List { type_ } items ->
            List.concatMap (List.map parseBlock) items
                |> (case type_ of
                        Markdown.Block.Unordered ->
                            UnOrderedList

                        Markdown.Block.Ordered _ ->
                            OrderedList
                   )
                |> List.singleton

        Markdown.Block.PlainInlines inlines ->
            PlainInline (List.map parseInline inlines |> FT.concat)
                |> List.singleton

        Markdown.Block.Custom _ blocks ->
            List.concatMap parseBlock blocks


parseInline : Markdown.Inline.Inline i -> FormattedText Markdown
parseInline inline =
    case inline of
        Markdown.Inline.Text string ->
            FT.fromString string

        Markdown.Inline.HardLineBreak ->
            FT.fromChar '\n'

        Markdown.Inline.CodeInline string ->
            FT.fromString string |> FT.formatAll Code

        Markdown.Inline.Link link _ subInlines ->
            List.map parseInline subInlines
                |> FT.concat
                |> FT.formatAll (Link link)

        Markdown.Inline.Image link _ subInlines ->
            -- An image is not really an inline element.
            -- We're just going to render it as a link.
            List.map parseInline subInlines
                |> FT.concat
                |> FT.formatAll (Link link)

        Markdown.Inline.HtmlInline _ _ subInlines ->
            List.map parseInline subInlines
                |> FT.concat

        Markdown.Inline.Emphasis 0 subInlines ->
            List.map parseInline subInlines
                |> FT.concat

        Markdown.Inline.Emphasis 1 subInlines ->
            List.map parseInline subInlines
                |> FT.concat
                |> FT.formatAll Italic

        Markdown.Inline.Emphasis 2 subInlines ->
            List.map parseInline subInlines
                |> FT.concat
                |> FT.formatAll Bold

        Markdown.Inline.Emphasis _ subInlines ->
            List.map parseInline subInlines
                |> FT.concat
                |> FT.formatAll Bold
                |> FT.formatAll Italic

        Markdown.Inline.Custom _ subInlines ->
            List.map parseInline subInlines
                |> FT.concat
