module FormattedText exposing (FormattedText, Range, addRange, all, any, append, chunks, concat, cons, contains, dropLeft, dropRight, empty, endsWith, filter, foldl, foldr, formatAll, formattedText, fromChar, fromList, fromString, indexes, indices, isEmpty, join, left, length, lines, map, pad, padLeft, padRight, ranges, repeat, reverse, right, slice, split, startsWith, text, toFloat, toInt, toList, toLower, toUpper, trees, trim, trimLeft, trimRight, unchunk, uncons, words)

{-| A type representing text with formatting.


## Type

@docs FormattedText


## Creation

@docs fromString
@docs unchunk
@docs formatAll


## Extraction

@docs text
@docs chunks
@docs trees


## String-like operations for FormattedText

@docs all
@docs any
@docs append
@docs concat
@docs cons
@docs contains
@docs dropLeft
@docs dropRight
@docs empty
@docs endsWith
@docs filter
@docs foldl
@docs foldr
@docs fromChar
@docs fromList
@docs indexes
@docs indices
@docs isEmpty
@docs join
@docs left
@docs length
@docs lines
@docs map
@docs pad
@docs padLeft
@docs padRight
@docs repeat
@docs reverse
@docs right
@docs slice
@docs split
@docs startsWith
@docs toFloat
@docs toInt
@docs toList
@docs toLower
@docs toUpper
@docs trim
@docs trimLeft
@docs trimRight
@docs uncons
@docs words


## Low-level

These functions provide a look behind the curtains at the ranges that are used to implement the FormattedText type.
You might need these functions to write encoders / decoders of the FormattedText type.
The FormattedText type is upholding some internal constraints, which mean that if you try to micromanage the exact ranges on it you're probably going to have a bad time.
If you feel the need to do this, please create an issue on our Github repo with your use case, we might be missing a function!

@docs Range
@docs formattedText
@docs addRange
@docs ranges

-}

import FormattedText.Internal as Internal
import FormattedText.Regex
import FormattedText.Shared as Shared
import FormattedText.Tree
import Regex exposing (Regex)


{-| Text with formatting.
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


{-| Extract all the markup ranges from the FormattedText.

This function is provided to enable encoding of the FormattedText type.
If you feel you need it to achieve something else, please share your use case in an issue on the FormattedText Github repo.
Your use case might be inspiration for an additional method.

-}
ranges : FormattedText markup -> List (Range markup)
ranges =
    Internal.ranges


{-| Format a range in the FormattedText with a certain type of markup.

Note that the FormattedText type obeys certain constraints (see documentation for the FormattedText type).
This means the `addRange` function makes no promises with regards to what you get back when you call `ranges`,
only that the characters in the range you indicated will have the formatting you specified attached to them.

-}
addRange : Range markup -> FormattedText markup -> FormattedText markup
addRange =
    Internal.addRange


{-| Append two FormattedTexts together. The equivalent of String.append or (++).
-}
append : FormattedText markup -> FormattedText markup -> FormattedText markup
append =
    Shared.append


{-| Get an empty FormattedText, the equivalent of an empty string.
-}
empty : FormattedText markup
empty =
    Shared.empty


{-| Concatenate a list of FormattedTexts together. The equivalent of String.concat.
-}
concat : List (FormattedText markup) -> FormattedText markup
concat =
    Shared.concat


{-| Length of the string in a FormattedText.
-}
length : FormattedText markup -> Int
length =
    Internal.text >> String.length


{-| Check if a FormattedText is empty. Same as `text >> String.isEmpty`.
-}
isEmpty : FormattedText markup -> Bool
isEmpty =
    text >> String.isEmpty


{-| Reverse the string in a FormattedText along with its formatting.
In other words: every single character will have the same markup after reversal as it had before.
-}
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


{-| Repeat a FormattedText a number of times. The equivalent of `String.repeat`.
-}
repeat : Int -> FormattedText markup -> FormattedText markup
repeat n formatted =
    List.repeat n formatted
        |> concat


{-| Prepend a character onto a FormattedText.
The added character will not be formatted.
The equivalent of `String.cons`.
-}
cons : Char -> FormattedText markup -> FormattedText markup
cons char formatted =
    append
        (String.fromChar char |> fromString)
        formatted


{-| Remove the first character from a FormattedText.
The equivalent of `String.uncons`.
-}
uncons : FormattedText markup -> Maybe ( Char, FormattedText markup )
uncons formatted =
    String.uncons (text formatted)
        |> Maybe.map
            (Tuple.mapSecond
                (\rest ->
                    formattedText
                        rest
                        (List.map (Shared.shift -1) (ranges formatted))
                )
            )


{-| Turn a single character into a FormattedText with no markup applied.
The equivalent of `String.fromChar`.
-}
fromChar : Char -> FormattedText markup
fromChar char =
    String.fromChar char
        |> fromString


{-| Split a FormattedText on a string. The equivalent of `String.split`.
Markup of all characters on the FormattedText will be preserved.
-}
split : String -> FormattedText markup -> List (FormattedText markup)
split splitter formatted =
    let
        indices : List Int
        indices =
            if splitter == "" then
                List.range 0 (length formatted - 1)
            else
                String.indices splitter (text formatted)
    in
    indices
        |> List.foldr (splitHelper <| String.length splitter) ( [], formatted )
        |> (\( splits, remainder ) ->
                if String.isEmpty splitter then
                    splits
                else
                    remainder :: splits
           )


splitHelper :
    Int
    -> Int
    -> ( List (FormattedText markup), FormattedText markup )
    -> ( List (FormattedText markup), FormattedText markup )
splitHelper splitterLength splitterIndex ( splits, formatted ) =
    let
        chunk : FormattedText markup
        chunk =
            dropLeft (splitterIndex + splitterLength) formatted

        remainder : FormattedText markup
        remainder =
            left splitterIndex formatted
    in
    ( chunk :: splits, remainder )


{-| Join a list FormattedTexts on a joining FormattedText.
This is the equivalent of `String.join`.
Markup on all the characters of the component FormattedTexts will be preserved.
-}
join : FormattedText markup -> List (FormattedText markup) -> FormattedText markup
join joiner parts =
    List.intersperse joiner parts
        |> concat


{-| Take the first n characters of a FormattedText, with their markup.
Equivalent of `String.left`.
-}
left : Int -> FormattedText markup -> FormattedText markup
left =
    Shared.left


{-| Take the last n characters of a FormattedText, with their markup.
Equivalent of `String.right`.
-}
right : Int -> FormattedText markup -> FormattedText markup
right =
    Shared.right


{-| Remove the first n characters of a FormattedText.
Equivalent of `String.dropLeft`.
-}
dropLeft : Int -> FormattedText markup -> FormattedText markup
dropLeft =
    Shared.dropLeft


{-| Remove the last n characters of a FormattedText.
Equivalent of `String.dropRight`.
-}
dropRight : Int -> FormattedText markup -> FormattedText markup
dropRight =
    Shared.dropRight


{-| Get a slice of a FormattedText as defined by a starting and ending index.
Equivalent of `String.slice`.
Each character in the slice will have the same markup it had in the original FormattedText.
-}
slice : Int -> Int -> FormattedText markup -> FormattedText markup
slice =
    Shared.slice


{-| Split a FormattedText on newline characters. The equivalent of `String.lines`.
-}
lines : FormattedText markup -> List (FormattedText markup)
lines formatted =
    split "\n" formatted


{-| Split a FormattedText on whitespace. The equivalent of `String.words`.
-}
words : FormattedText markup -> List (FormattedText markup)
words formatted =
    FormattedText.Regex.split
        Regex.All
        (Regex.regex "\\s+")
        (trim formatted)


{-| Trim whitespace from both ends of a FormattedText. The equivalent of `String.trim`.
-}
trim : FormattedText markup -> FormattedText markup
trim =
    trimLeft >> trimRight


{-| Trim whitespace from the left end of a FormattedText. The equivalent of `String.trimLeft`.
-}
trimLeft : FormattedText markup -> FormattedText markup
trimLeft formatted =
    FormattedText.Regex.replace
        (Regex.AtMost 1)
        (Regex.regex "^\\s+")
        (always empty)
        formatted


{-| Trim whitespace from the right end of a FormattedText. The equivalent of `String.trimRight`.
-}
trimRight : FormattedText markup -> FormattedText markup
trimRight formatted =
    FormattedText.Regex.replace
        (Regex.AtMost 1)
        (Regex.regex "\\s+$")
        (always empty)
        formatted


{-| Create a FormattedText from a string and a list of markup ranges.

Note that the FormattedText type obeys certain constraints (see documentation for the FormattedText type).
This means the `addRange` function makes no promises with regards to what you get back when you call `ranges`,
only that the characters in the range you indicated will have the formatting you specified attached to them.

-}
formattedText : String -> List (Range markup) -> FormattedText markup
formattedText =
    Shared.formattedText


{-| Apply some formatting to the entirety of a piece of formatted text.
-}
formatAll : markup -> FormattedText markup -> FormattedText markup
formatAll tag formatted =
    Internal.addRange { tag = tag, start = 0, end = length formatted } formatted


{-| Get indexes of occurences of a sub-FormattdText inside a larger one.
Both text and markup of the contained occurrence need to match the sub-FormattedText.
The equivalent of `String.indexes`.
-}
indexes : FormattedText markup -> FormattedText markup -> List Int
indexes part whole =
    let
        partAtIndex : Int -> FormattedText markup
        partAtIndex index =
            slice index (index + length part) whole
    in
    String.indexes (text part) (text whole)
        -- We found all matching substrings, but they might have different formatting.
        |> List.filter (partAtIndex >> equal part)


{-| Alias of `indexes`.
-}
indices : FormattedText markup -> FormattedText markup -> List Int
indices =
    indexes


{-| Checks if a FormattedText contains a sub-FormattedText.
Both text and markup of the contained occurrence need to match the sub-FormattedText.
Equivalent of `String.contains`.
-}
contains : FormattedText markup -> FormattedText markup -> Bool
contains part whole =
    if isEmpty part then
        True
    else
        not <| List.isEmpty (indexes part whole)


{-| Check if a FormattedText starts with a sub-FormattedText.
Both text and markup of the contained occurence need to match the sub-FormattedText.
Equivalent of `String.startsWith`.
-}
startsWith : FormattedText markup -> FormattedText markup -> Bool
startsWith start whole =
    equal start (left (length start) whole)


{-| Check if a FormattedText end with a sub-FormattedText.
Both text and markup of the contained occurence need to match the sub-FormattedText.
Equivalent of `String.endsWith`.
-}
endsWith : FormattedText markup -> FormattedText markup -> Bool
endsWith end whole =
    equal end (right (length end) whole)


equal : FormattedText markup -> FormattedText markup -> Bool
equal formattedA formattedB =
    let
        textEqual : Bool
        textEqual =
            text formattedA == text formattedB

        rangesEqual : Bool
        rangesEqual =
            sortRanges (ranges formattedA) == sortRanges (ranges formattedB)

        sortRanges : List (Range markup) -> List (Range markup)
        sortRanges ranges =
            List.sortBy hashRange ranges

        hashRange : Range markup -> String
        hashRange { start, end, tag } =
            -- Because we have no information about what tag might be,
            -- stringifying it is our only resort for allowing us to compare it.
            toString ( start, end, tag )
    in
    textEqual && rangesEqual


{-| Parse the FormattedText as an Int.
Markup has no effect on the result.
The equivalent of `String.toInt`.
-}
toInt : FormattedText markup -> Result String Int
toInt formatted =
    String.toInt (text formatted)


{-| Parse the FormattedText as a Float.
Markup has no effect on the result.
The equivalent of `String.toInt`.
-}
toFloat : FormattedText markup -> Result String Float
toFloat formatted =
    String.toFloat (text formatted)


{-| Turn a FormattedText into a list of Chars.
Markup is discarded.
The equivalent of `String.toList`.
-}
toList : FormattedText markup -> List Char
toList formatted =
    String.toList (text formatted)


{-| Generate a FormattedText from a list of Chars.
The resulting FormattedText will not contain any markup.
This is the equivalent of `String.fromList`.
-}
fromList : List Char -> FormattedText markup
fromList chars =
    fromString (String.fromList chars)


{-| Uppercase the text in a FormattedText.
Markup will not be affected.
This is the equivalent of `String.toUpper`.
-}
toUpper : FormattedText markup -> FormattedText markup
toUpper formatted =
    formattedText
        (String.toUpper <| text formatted)
        (ranges formatted)


{-| Lowercase the text in a FormattedText.
Markup will not be affected.
This is the equivalent of `String.toLowr`.
-}
toLower : FormattedText markup -> FormattedText markup
toLower formatted =
    formattedText
        (String.toLower <| text formatted)
        (ranges formatted)


{-| Pad a FormattedText on the left with a character, until the length of the FormattedText matches or exceeds a certain length.
You can provide markup to be applied to the padding.
This is the equivalent of `String.padLeft`.
-}
padLeft : Int -> Char -> List markup -> FormattedText markup -> FormattedText markup
padLeft upTo char markups formatted =
    let
        amount : Int
        amount =
            upTo - length formatted
    in
    append (createPadding amount char markups) formatted


{-| Pad a FormattedText on the right with a character, until the length of the FormattedText matches or exceeds a certain length.
You can provide markup to be applied to the padding.
This is the equivalent of `String.padRight`.
-}
padRight : Int -> Char -> List markup -> FormattedText markup -> FormattedText markup
padRight upTo char markups formatted =
    let
        amount : Int
        amount =
            upTo - length formatted
    in
    append formatted (createPadding amount char markups)


{-| Pad a FormattedText on both sides with a character, until the length of the FormattedText matches or exceeds a certain length.
You can provide markup to be applied to the padding.
This is the equivalent of `String.pad`.
-}
pad : Int -> Char -> List markup -> FormattedText markup -> FormattedText markup
pad upTo char markups formatted =
    let
        ln : Int
        ln =
            length formatted

        rightAmount : Int
        rightAmount =
            (upTo - ln)
                // 2
                |> max 0

        leftAmount : Int
        leftAmount =
            (upTo - ln) - rightAmount
    in
    concat
        [ createPadding leftAmount char markups
        , formatted
        , createPadding rightAmount char markups
        ]


createPadding : Int -> Char -> List markup -> FormattedText markup
createPadding amount char markups =
    let
        paddingString : String
        paddingString =
            String.repeat amount (String.fromChar char)
    in
    List.foldl formatAll (fromString paddingString) markups


{-| Modify each character of a FormattedText by passing it through a mapping function.
This will not affect markup, which is to say: the character returned by the mapping function will have the same markup applied to it that the character passed into the mapping function had applied to it.
This is the equivalent of `String.map`.
-}
map : (Char -> Char) -> FormattedText markup -> FormattedText markup
map fn formatted =
    formattedText
        (String.map fn (text formatted))
        (ranges formatted)


{-| Filter out characters from a FormattedText.
This is the equivalent of `String.filter`.
-}
filter : (Char -> Bool) -> FormattedText markup -> FormattedText markup
filter predicate formatted =
    let
        keepCharString : FormattedText markup -> Bool
        keepCharString charString =
            String.uncons (text charString)
                |> Maybe.map (Tuple.first >> predicate)
                |> Maybe.withDefault False
    in
    split "" formatted
        |> List.filter keepCharString
        |> concat


{-| Fold from the left over a Formattedtext.
This is the equivalent of `String.foldl`.
-}
foldl : (Char -> b -> b) -> b -> FormattedText markup -> b
foldl foldfn start formatted =
    text formatted
        |> String.foldl foldfn start


{-| Fold from the right over a Formattedtext.
This is the equivalent of `String.foldr`.
-}
foldr : (Char -> b -> b) -> b -> FormattedText markup -> b
foldr foldfn start formatted =
    text formatted
        |> String.foldr foldfn start


{-| Check if any character in a FormattedText meets a condition.
This is the equivalent of `String.any`.
-}
any : (Char -> Bool) -> FormattedText markup -> Bool
any predicate formatted =
    text formatted
        |> String.any predicate


{-| Check if all characters in a FormattedText meets a condition.
This is the equivalent of `String.all`.
-}
all : (Char -> Bool) -> FormattedText markup -> Bool
all predicate formatted =
    text formatted
        |> String.all predicate


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


{-| Certain operations, like rendering to Html, can be hard to perform with FormattedText directly.
`chunks` splits a FormattedText up into chunks with equal markup.
It's up to you to define what you want your chunks to look like.
You can get tuples of Strings and markup by passing `(,)`, or you can render directly into Html!
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


{-| Certain operations, like rendering to Html, can be hard to perform with FormattedText directly.
`trees` creates a markup tree from a FormattedText.
You can pass it a function for generating a text leaf of the tree, and a function to generate a markup node of the tree.
-}
trees : (String -> tree) -> (markup -> List tree -> tree) -> FormattedText markup -> List tree
trees =
    FormattedText.Tree.trees
