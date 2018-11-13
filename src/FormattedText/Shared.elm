module FormattedText.Shared exposing (Range, append, concat, dropLeft, dropRight, empty, formattedText, left, length, right, shift, slice)

{-| Functions extracted from the `FormattedText` module to break a circular dependency between it and `FormattedText.Regex`.
-}

import FormattedText.Internal exposing (FormattedText, addRange, fromString, ranges, text)


type alias Range markup =
    { tag : markup
    , start : Int
    , end : Int
    }


append : FormattedText markup -> FormattedText markup -> FormattedText markup
append formattedA formattedB =
    let
        textA : String
        textA =
            text formattedA

        textB : String
        textB =
            text formattedB

        rangesA : List (Range markup)
        rangesA =
            ranges formattedA

        rangesB : List (Range markup)
        rangesB =
            ranges formattedB

        shiftLength : Int
        shiftLength =
            String.length textA

        combinedRanges : List (Range markup)
        combinedRanges =
            rangesA
                ++ List.map (shift shiftLength) rangesB
    in
    fromString (textA ++ textB)
        |> (\formatted -> List.foldl addRange formatted combinedRanges)


empty : FormattedText markup
empty =
    fromString ""


length : FormattedText markup -> Int
length =
    text >> String.length


left : Int -> FormattedText markup -> FormattedText markup
left n formatted =
    formattedText
        (text formatted |> String.left n)
        (ranges formatted)


right : Int -> FormattedText markup -> FormattedText markup
right n formatted =
    let
        shiftLength =
            max 0 n - length formatted
    in
    formattedText
        (text formatted |> String.right n)
        (ranges formatted |> List.map (shift shiftLength))


dropLeft : Int -> FormattedText markup -> FormattedText markup
dropLeft n formatted =
    right (length formatted - n) formatted


dropRight : Int -> FormattedText markup -> FormattedText markup
dropRight n formatted =
    left (length formatted - n) formatted


slice : Int -> Int -> FormattedText markup -> FormattedText markup
slice start end formatted =
    let
        ln : Int
        ln =
            length formatted

        fixBound : Int -> Int
        fixBound n =
            if n < 0 then
                ln + n

            else
                n

        leftBound : Int
        leftBound =
            fixBound start

        rightBound : Int
        rightBound =
            ln - fixBound end
    in
    formatted
        |> dropRight rightBound
        |> dropLeft leftBound


formattedText : String -> List (Range markup) -> FormattedText markup
formattedText text ranges =
    List.foldr addRange (fromString text) ranges


shift : Int -> Range markup -> Range markup
shift amount range =
    { range
        | start = range.start + amount
        , end = range.end + amount
    }


concat : List (FormattedText markup) -> FormattedText markup
concat xs =
    List.foldr append empty xs
