# FormattedText

The `FormattedText` type represents some text with markup applied to it. The goal of this library is to make working with such text as easy as working with strings.

## Creating FormattedText
You can create a FormattedText in a number of ways.

- Use `formattedText` to combine a string with some formatting ranges. Ranges are allowed to overlap.
- Use `fromString` to turn a string into an unformatted FormattedText.
- Use `unchunk` to create a FormattedText from a list of chunks, pieces of text with homogeneous formatting.

## Manipulating FormattedText
Anything you can do with Strings you can do with FormattedText too,
because this lib comes with equivalents of all functions from the core String and Regex modules.

## Rendering FormattedText
The easiest way to render `FormattedText` into Html is using the `trees` function, which builds a markup tree from your `FormattedText`.

```elm
import FormattedText exposing (..)
import Html exposing (Html)

type Markup = Bold | Italic

view : FormattedText Markup -> Html msg
view formattedText =
    Html.p [] (trees Html.text viewMarkup)

viewMarkup : Markup -> List (Html msg) -> Html msg
viewMarkup markup children =
    case markup of
        Bold ->
            Html.strong [] children

        Italic ->
            Html.i [] children
```
