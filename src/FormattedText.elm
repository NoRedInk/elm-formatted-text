module FormattedText exposing (FormattedText, Range, addRange, append, chunks, concat, cons, empty, formatAll, formattedText, fromChar, fromString, isEmpty, length, ranges, repeat, reverse, text, unchunk, uncons)

{-| A type representing text with formatting.


## Types

@docs FormattedText, Range


## Creation and extraction

@docs formattedText, text, fromString, ranges, chunks, unchunk, formatAll, addRange


## String equivalent operations

@docs empty, append, concat, length, isEmpty, reverse, repeat, cons, uncons, fromChar

-}

import FormattedText.Internal as Internal


{-| Text with formatting.

Touching and overlapping ranges of formatting using the same markup will automatically be merged into one.
If this is not what you want, consider using a parameterised markup type:

    type Tag
        = Tag Int

-}
type alias FormattedText markup =
    Internal.FormattedText markup


{-| A format applied to a range.
Formatting applies from the start index up to but excluding the end index.
-}
type alias Range markup =
    { tag : markup
    , start : Int
    , end : Int
    }


{-| Turn a plain string into formatted text.
-}
fromString : String -> FormattedText markup
fromString =
    Internal.fromString


{-| Extract the plain string from formatted text.
-}
text : FormattedText markup -> String
text =
    Internal.text


{-| Extract the ranges string from formatted text.
-}
ranges : FormattedText markup -> List (Range markup)
ranges =
    Internal.ranges


{-| Add extra formatting to formatted text.
-}
addRange : Range markup -> FormattedText markup -> FormattedText markup
addRange =
    Internal.addRange


{-| -}
append : FormattedText markup -> FormattedText markup -> FormattedText markup
append formattedA formattedB =
    let
        textA : String
        textA =
            Internal.text formattedA

        textB : String
        textB =
            Internal.text formattedB

        rangesA : List (Range markup)
        rangesA =
            Internal.ranges formattedA

        rangesB : List (Range markup)
        rangesB =
            Internal.ranges formattedB

        shiftLength : Int
        shiftLength =
            String.length textA

        combinedRanges : List (Range markup)
        combinedRanges =
            rangesA
                ++ List.map (shift shiftLength) rangesB
    in
    Internal.fromString (textA ++ textB)
        |> (\formattedText -> List.foldl Internal.addRange formattedText combinedRanges)


shift : Int -> Range markup -> Range markup
shift amount range =
    { range
        | start = range.start + amount
        , end = range.end + amount
    }


{-| -}
empty : FormattedText markup
empty =
    Internal.fromString ""


{-| -}
concat : List (FormattedText markup) -> FormattedText markup
concat xs =
    List.foldr append empty xs


{-| -}
length : FormattedText markup -> Int
length =
    Internal.text >> String.length


{-| -}
isEmpty : FormattedText markup -> Bool
isEmpty =
    text >> String.isEmpty


{-| -}
reverse : FormattedText markup -> FormattedText markup
reverse formatted =
    let
        inputLength : Int
        inputLength =
            length formatted

        reverseRange : Range markup -> Range markup
        reverseRange range =
            { tag = range.tag
            , start = inputLength - range.end
            , end = inputLength - range.start
            }
    in
    formattedText
        (text formatted |> String.reverse)
        (ranges formatted |> List.map reverseRange)


{-| -}
repeat : Int -> FormattedText markup -> FormattedText markup
repeat n formatted =
    List.repeat n formatted
        |> concat


{-| -}
cons : Char -> FormattedText markup -> FormattedText markup
cons char formatted =
    append
        (String.fromChar char |> fromString)
        formatted


{-| -}
uncons : FormattedText markup -> Maybe ( Char, FormattedText markup )
uncons formatted =
    String.uncons (text formatted)
        |> Maybe.map
            (Tuple.mapSecond
                (\rest ->
                    formattedText
                        rest
                        (List.map (shift -1) (ranges formatted))
                )
            )


{-| -}
fromChar : Char -> FormattedText markup
fromChar char =
    String.fromChar char
        |> fromString


{-| -}
formattedText : String -> List (Range markup) -> FormattedText markup
formattedText text ranges =
    List.foldr Internal.addRange (Internal.fromString text) ranges


{-| Apply some formatting to the entirety of a piece of formatted text.
-}
formatAll : markup -> FormattedText markup -> FormattedText markup
formatAll tag formattedText =
    Internal.addRange { tag = tag, start = 0, end = length formattedText } formattedText


{-| Helper type for the chunks function.

This is either the beginning or end of a range with particular formatting.

-}
type RangeBoundary markup
    = Start markup
    | End markup


{-| Helper type for the chunks function.

A position at which the markup of formatted text changes, indicating the boundary of a chunk.
Formatting changes either because a markup range starts or ends at this position.

-}
type alias ChunkSeperator markup =
    ( Int, RangeBoundary markup )


{-| Helper type for the chunks function.
-}
type alias FoldState markup chunk =
    { text : String
    , openTags : List markup
    , chunks : List chunk
    }


{-| Helper for the chunks function.
-}
rangeBounds : List (Range markup) -> List (ChunkSeperator markup)
rangeBounds ranges =
    let
        forRange : Range markup -> List (ChunkSeperator markup)
        forRange { tag, start, end } =
            [ ( start, Start tag ), ( end, End tag ) ]
    in
    ranges
        |> List.concatMap forRange
        |> List.sortBy Tuple.first


{-| FormattedText can be hard to work with directly. `chunks` splits it up into sections with equal markup applied.
-}
chunks : (String -> List markup -> chunk) -> FormattedText markup -> List chunk
chunks toChunk formatted =
    let
        startRangeBounds : List (ChunkSeperator markup)
        startRangeBounds =
            Internal.ranges formatted
                |> rangeBounds

        startFoldState : FoldState markup chunk
        startFoldState =
            FoldState (Internal.text formatted) [] []

        addChunk : String -> List markup -> List chunk -> List chunk
        addChunk text tags chunks =
            if text == "" then
                chunks
            else
                toChunk text tags
                    :: chunks

        splitOffRightMostChunk : ChunkSeperator markup -> FoldState markup chunk -> FoldState markup chunk
        splitOffRightMostChunk ( position, rangeBoundary ) { text, openTags, chunks } =
            let
                chunkText : String
                chunkText =
                    String.dropLeft position text

                remainingText : String
                remainingText =
                    String.left position text

                newOpenTags : List markup
                newOpenTags =
                    case rangeBoundary of
                        -- Because we're moving through the data right-to-left,
                        -- `End` means we're entering a range and `Start` means we're exiting one.
                        End tag ->
                            tag :: openTags

                        Start tag ->
                            List.filter (not << (==) tag) openTags

                newChunks : List chunk
                newChunks =
                    addChunk chunkText openTags chunks
            in
            FoldState remainingText newOpenTags newChunks
    in
    {- We're going to break apart our text piece by piece, splitting it on chunk separators.
       Suppose we start with the following piece of formatted text:

           "Whales are a group of marine mammals"
            [     )               [             )
            0     9               22            36

       When we split up the right most chunk we're left with this:

           "Whales are a group of "
            [     )
            0     9

       We continue by splitting of the right most chunk until nothing is left.
       Notice how if we'd split off fragments from the left we'd need to shift the positions
       of the remaining chunk separators by the length of the text removed after each step.
    -}
    List.foldr splitOffRightMostChunk startFoldState startRangeBounds
        -- Create the last chunk with the final piece of unformatted text.
        |> (\{ chunks, text } -> addChunk text [] chunks)


{-| Take a list of text chunks with formatting and turn them into formatted text.
-}
unchunk : List ( String, List markup ) -> FormattedText markup
unchunk chunks =
    let
        formatChunk : ( String, List markup ) -> FormattedText markup
        formatChunk ( text, tags ) =
            List.foldr formatAll (Internal.fromString text) tags
    in
    chunks
        |> List.map formatChunk
        |> concat
