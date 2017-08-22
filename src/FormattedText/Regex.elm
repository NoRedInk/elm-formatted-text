module FormattedText.Regex exposing (Match, contains, find, replace, split)

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
            case splitAround match formatted of
                ( before, after ) ->
                    concat [ before, replacer match, after ]
    in
    find howMany regex formatted
        -- It's important to fold from the right here.
        -- Each replacement potentially changes the length of the text,
        -- affecting indexes of other matches to the right.
        -- By starting from the right most match and working back this won't become a problem.
        |> List.foldr replaceMatch formatted


{-| -}
split : Regex.HowMany -> Regex -> FormattedText markup -> List (FormattedText markup)
split howMany regex formatted =
    let
        splitAroundMatch :
            Match markup
            -> ( FormattedText markup, List (FormattedText markup) )
            -> ( FormattedText markup, List (FormattedText markup) )
        splitAroundMatch match ( remaining, chunks ) =
            case splitAround match remaining of
                ( before, after ) ->
                    ( before, after :: chunks )

        -- `Regex.split` treats negative `AtMost` values as `All`, `Regex.find` treats them as `AtMost 0.
        -- Because we want to implement the `split behaviour` using `find` we need to transform.
        fixedHowMany : Regex.HowMany
        fixedHowMany =
            case howMany of
                Regex.All ->
                    howMany

                Regex.AtMost n ->
                    if n < 0 then
                        Regex.All
                    else
                        howMany
    in
    find fixedHowMany regex formatted
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


{-| -}
contains : Regex -> FormattedText markup -> Bool
contains regex formatted =
    Regex.contains regex (text formatted)
