module Spec.FormattedText.Tree exposing (..)

import Expect
import FormattedText as FT
import FormattedText.Fuzz
import FormattedText.Tree
import Fuzz
import Test exposing (..)


type Tree markup
    = Leaf String
    | Node markup (List (Tree markup))


suite : Test
suite =
    describe "FormattedText.Tree"
        [ describe "mapAccumL"
            [ fuzz (Fuzz.list Fuzz.int) "does as map does" <|
                \ints ->
                    FormattedText.Tree.mapAccumL (\n () -> ( (), n * 2 )) () ints
                        |> Tuple.second
                        |> Expect.equal (List.map (\n -> n * 2) ints)
            , fuzz (Fuzz.list Fuzz.unit) "passes through an accumumlator" <|
                \units ->
                    FormattedText.Tree.mapAccumL (\() acc -> ( acc + 1, acc )) 1 units
                        |> Tuple.second
                        |> Expect.equal (List.range 1 (List.length units))
            ]
        , test "creates the correct tree for nested markup (1)" <|
            \_ ->
                FT.concat
                    [ FT.fromString "Nest "
                    , FT.concat
                        [ FT.fromString "Yellow"
                            |> FT.formatAll FormattedText.Fuzz.Yellow
                        , FT.fromString " inside of Red"
                        ]
                        |> FT.formatAll FormattedText.Fuzz.Red
                    , FT.fromString "."
                    ]
                    |> FormattedText.Tree.trees identity toXmlNode
                    |> String.concat
                    |> Expect.equal "Nest <red><yellow>Yellow</yellow> inside of Red</red>."
        , test "creates the correct tree for nested markup (2)" <|
            \_ ->
                FT.concat
                    [ FT.concat
                        [ FT.fromString "a"
                            |> FT.formatAll FormattedText.Fuzz.Yellow
                        , FT.fromString "b"
                        , FT.fromString "c"
                            |> FT.formatAll FormattedText.Fuzz.Yellow
                        ]
                        |> FT.formatAll FormattedText.Fuzz.Red
                    , FT.fromString "d"
                    ]
                    |> FormattedText.Tree.trees identity toXmlNode
                    |> String.concat
                    |> Expect.equal "<red><yellow>a</yellow>b<yellow>c</yellow></red>d"
        , fuzz FormattedText.Fuzz.formattedText "trees and fromTrees are duals" <|
            \formatted ->
                FormattedText.Tree.trees Leaf Node formatted
                    |> fromTrees
                    |> FormattedText.Fuzz.equals formatted
        , test "trees and fromTrees are duals (known hard case)" <|
            \_ ->
                let
                    formatted =
                        FT.fromString "abcd"
                            |> FT.addRange { tag = FormattedText.Fuzz.Red, start = 1, end = 3 }
                            |> FT.addRange { tag = FormattedText.Fuzz.Green, start = 1, end = 3 }
                            |> FT.addRange { tag = FormattedText.Fuzz.Blue, start = 0, end = 2 }
                in
                FormattedText.Tree.trees Leaf Node formatted
                    |> fromTrees
                    |> FormattedText.Fuzz.equals formatted
        ]


toXmlNode : FormattedText.Fuzz.Markup -> List String -> String
toXmlNode markup children =
    "<" ++ tag markup ++ ">" ++ String.concat children ++ "</" ++ tag markup ++ ">"


tag : FormattedText.Fuzz.Markup -> String
tag markup =
    case markup of
        FormattedText.Fuzz.Red ->
            "red"

        FormattedText.Fuzz.Blue ->
            "blue"

        FormattedText.Fuzz.Green ->
            "green"

        FormattedText.Fuzz.Yellow ->
            "yellow"


dual : Test
dual =
    fuzz FormattedText.Fuzz.formattedText "rangeForest doesn't mangle ranges" <|
        \formatted ->
            FormattedText.Tree.rangeForest formatted
                |> List.concatMap toRanges
                |> List.foldl FT.addRange (FT.fromString (FT.text formatted))
                |> FormattedText.Fuzz.equals formatted


fromTrees : List (Tree markup) -> FT.FormattedText markup
fromTrees trees =
    List.map fromTree trees
        |> FT.concat


fromTree : Tree markup -> FT.FormattedText markup
fromTree tree =
    case tree of
        Leaf text ->
            FT.fromString text

        Node markup subtrees ->
            FT.formatAll markup (fromTrees subtrees)


toRanges : FormattedText.Tree.Tree (FormattedText.Tree.Range markup) -> List (FormattedText.Tree.Range markup)
toRanges (FormattedText.Tree.Tree range children) =
    range :: List.concatMap toRanges children


nonEmptyString : Fuzz.Fuzzer String
nonEmptyString =
    Fuzz.map2 String.cons Fuzz.char Fuzz.string
