module FormattedText.Regex exposing (Match, find, replace)

{-| Regex operations for FormattedText

@docs Match, find, replace

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


{-| -}
find : Regex.HowMany -> Regex -> FormattedText markup -> List (Match markup)
find howMany regex formatted =
    text formatted
        |> Regex.find howMany regex
        |> List.map (fromStringMatch formatted)


fromStringMatch : FormattedText markup -> Regex.Match -> Match markup
fromStringMatch fullFormatted { match, index, number } =
    { match = slice index (index + String.length match) fullFormatted
    , index = index
    , number = number
    }


{-| -}
replace :
    Regex.HowMany
    -> Regex
    -> (Match markup -> FormattedText markup)
    -> FormattedText markup
    -> FormattedText markup
replace howMany regex replacer formatted =
    let
        replaceMatch : Match markup -> FormattedText markup -> FormattedText markup
        replaceMatch match formatted =
            replaceSlice
                match.index
                (match.index + length match.match)
                (replacer match)
                formatted
    in
    find howMany regex formatted
        -- It's important to fold from the right here.
        -- Each replacement potentially changes the length of the text,
        -- affecting indexes of other matches to the right.
        -- By starting from the right most match and working back this won't become a problem.
        |> List.foldr replaceMatch formatted


replaceSlice : Int -> Int -> FormattedText markup -> FormattedText markup -> FormattedText markup
replaceSlice start end part whole =
    let
        first : FormattedText markup
        first =
            left start whole

        last : FormattedText markup
        last =
            dropLeft end whole
    in
    concat [ first, part, last ]
