module FormattedText.Regex exposing
    ( Match
    , contains
    , find
    , replace
    , split
    )

{-| Regex operations for FormattedText

@docs Match
@docs contains
@docs find
@docs replace
@docs split

-}

import FormattedText.Internal exposing (FormattedText, text)
import FormattedText.Shared exposing (concat, dropLeft, left, length, slice)
import Regex exposing (Regex)


{-| A match in a piece of FormattedText

This is the equivalent for FormattedTexts of `Regex.Match` for Strings.
Unfortunately submatches cannot currently be supported when matching formatted texts.

-}
type alias Match markup =
    { match : FormattedText markup
    , index : Int
    , number : Int
    }


{-| Find regex matches in a FormattedText.
This is the equivalent of `Regex.find`.
-}
find : Regex -> FormattedText markup -> List (Match markup)
find regex formatted =
    text formatted
        |> Regex.find regex
        |> List.map (fromStringMatch formatted)


fromStringMatch : FormattedText markup -> Regex.Match -> Match markup
fromStringMatch fullFormatted { match, index, number } =
    { match = slice index (index + String.length match) fullFormatted
    , index = index
    , number = number
    }


{-| Replace regex matches in a FormattedText.
This is the equivalent of `Regex.replace`.
-}
replace :
    Regex
    -> (Match markup -> FormattedText markup)
    -> FormattedText markup
    -> FormattedText markup
replace regex replacer formatted =
    let
        replaceMatch : Match markup -> FormattedText markup -> FormattedText markup
        replaceMatch match formatted_ =
            case splitAround match formatted_ of
                ( before, after ) ->
                    concat [ before, replacer match, after ]
    in
    find regex formatted
        -- It's important to fold from the right here.
        -- Each replacement potentially changes the length of the text,
        -- affecting indexes of other matches to the right.
        -- By starting from the right most match and working back this won't become a problem.
        |> List.foldr replaceMatch formatted


{-| Split a FormattedText on matches with a regex.
This is the equivalent of `Regex.split`.
-}
split : Regex -> FormattedText markup -> List (FormattedText markup)
split regex formatted =
    let
        splitAroundMatch :
            Match markup
            -> ( FormattedText markup, List (FormattedText markup) )
            -> ( FormattedText markup, List (FormattedText markup) )
        splitAroundMatch match ( remaining, chunks ) =
            case splitAround match remaining of
                ( before, after ) ->
                    ( before, after :: chunks )
    in
    find regex formatted
        -- It's important to fold from the right here.
        -- By chopping of chunks from the right side,
        -- the indexes of other matches to the left remain valid.
        |> List.foldr splitAroundMatch ( formatted, [] )
        |> (\( last, chunks ) -> last :: chunks)


splitAround : Match markup -> FormattedText markup -> ( FormattedText markup, FormattedText markup )
splitAround { match, index } formatted =
    ( left index formatted
    , dropLeft (index + length match) formatted
    )


{-| Check if a FormattedText contains matches with a regex.
This is the equivalent of `Regex.contains`.
-}
contains : Regex -> FormattedText markup -> Bool
contains regex formatted =
    Regex.contains regex (text formatted)
