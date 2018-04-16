module Spec.FormattedText.Tree exposing (..)

import FormattedText as FT
import FormattedText.Fuzz
import FormattedText.Tree
import Test exposing (..)


dual : Test
dual =
    fuzz FormattedText.Fuzz.formattedText "rangeForest doesn't mangle ranges" <|
        \formatted ->
            FormattedText.Tree.rangeForest formatted
                |> List.concatMap toRanges
                |> List.foldl FT.addRange (FT.fromString (FT.text formatted))
                |> FormattedText.Fuzz.equals formatted


toRanges : FormattedText.Tree.Tree (FormattedText.Tree.Range markup) -> List (FormattedText.Tree.Range markup)
toRanges (FormattedText.Tree.Tree range children) =
    range :: List.concatMap toRanges children
