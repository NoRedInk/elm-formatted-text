module FormattedText.Tree exposing (..)

import AllenAlgebra
import FormattedText.Internal as Internal exposing (FormattedText)
import Parser


trees : (String -> tree) -> (markup -> List tree -> tree) -> FormattedText markup -> List tree
trees leaf node formatted =
    rangeForest formatted
        |> toParser { leaf = leaf, node = node }
        |> Parser.parse (Internal.text formatted)


type Tree a
    = Tree a (Forest a)


type alias Forest a =
    List (Tree a)


{-| A simplification of the allen relations, keeping only information we're
interested in.
-}
type RangeRelation
    = Equal
    | Contains
    | During
    | Before
    | After
    | OverlapsLeft
    | OverlapsRight


relation : Range a -> Range b -> RangeRelation
relation range1 range2 =
    case AllenAlgebra.relation range1 range2 of
        AllenAlgebra.Overlaps ->
            OverlapsLeft

        AllenAlgebra.OverlapsInverse ->
            OverlapsRight

        AllenAlgebra.During ->
            During

        AllenAlgebra.DuringInverse ->
            Contains

        AllenAlgebra.Meets ->
            Before

        AllenAlgebra.MeetsInverse ->
            After

        AllenAlgebra.Starts ->
            During

        AllenAlgebra.StartsInverse ->
            Contains

        AllenAlgebra.Finishes ->
            During

        AllenAlgebra.FinishesInverse ->
            Contains

        AllenAlgebra.Before ->
            Before

        AllenAlgebra.After ->
            After

        AllenAlgebra.Equal ->
            Equal


type alias Range tag =
    { tag : tag
    , start : Int
    , end : Int
    }


addTree : Tree (Range tag) -> Forest (Range tag) -> Forest (Range tag)
addTree (Tree newRange newChildren) forest =
    case forest of
        [] ->
            [ Tree newRange newChildren ]

        (Tree headRange headChildren) :: rest ->
            case relation newRange headRange of
                Equal ->
                    Tree newRange (addTree (Tree headRange headChildren) newChildren) :: rest

                Contains ->
                    Tree newRange (addTree (Tree headRange headChildren) newChildren) :: rest

                During ->
                    Tree headRange (addTree (Tree newRange newChildren) headChildren) :: rest

                Before ->
                    Tree newRange newChildren :: Tree headRange headChildren :: rest

                After ->
                    Tree headRange headChildren :: addTree (Tree newRange newChildren) rest

                OverlapsLeft ->
                    let
                        ( outsideHead, insideHead ) =
                            splitOn headRange.start newRange
                    in
                    Tree outsideHead []
                        :: Tree headRange (addTree (Tree insideHead []) headChildren)
                        :: rest
                        |> (\newForest -> List.foldl addTree newForest newChildren)

                OverlapsRight ->
                    let
                        ( insideHead, outsideHead ) =
                            splitOn headRange.end newRange
                    in
                    Tree headRange (addTree (Tree insideHead []) headChildren)
                        :: Tree outsideHead []
                        :: rest
                        |> (\newForest -> List.foldl addTree newForest newChildren)


splitOn : Int -> Range tag -> ( Range tag, Range tag )
splitOn n range =
    ( { range | end = n }
    , { range | start = n }
    )


type alias TreeBuilder markup tree =
    { leaf : String -> tree
    , node : markup -> List tree -> tree
    }


rangeForest : FormattedText markup -> Forest (Range markup)
rangeForest formatted =
    Internal.ranges formatted
        |> List.map (\range -> Tree range [])
        |> List.foldl addTree []


toParser : TreeBuilder markup tree -> Forest (Range markup) -> Parser.Parser tree
toParser treeBuilder forest =
    toParser_ treeBuilder 0 forest
        |> Tuple.second


toParser_ : TreeBuilder markup tree -> Int -> Forest (Range markup) -> ( Int, Parser.Parser tree )
toParser_ treeBuilder offset forest =
    mapAccumL (bitesForRange treeBuilder) offset forest
        |> Tuple.mapSecond
            (\nestedBites ->
                { bites = List.concatMap identity nestedBites
                , digestRemainder = treeBuilder.leaf
                }
            )


bitesForRange : TreeBuilder markup tree -> Tree (Range markup) -> Int -> ( Int, List (Parser.Bite tree) )
bitesForRange treeBuilder (Tree range children) offset =
    ( range.end
    , List.filterMap identity
        [ case range.start - offset of
            0 ->
                Nothing

            chars ->
                Just
                    { chars = chars
                    , digest = treeBuilder.leaf
                    }
        , Just
            { chars = range.end - range.start
            , digest = digestNode treeBuilder (Tree range children)
            }
        ]
    )


digestNode : TreeBuilder markup tree -> Tree (Range markup) -> String -> tree
digestNode treeBuilder (Tree range children) string =
    toParser_ treeBuilder range.start children
        |> Tuple.second
        |> Parser.parse string
        |> treeBuilder.node range.tag


mapAccumL : (a -> c -> ( c, b )) -> c -> List a -> ( c, List b )
mapAccumL mapFn init xs =
    let
        go : a -> ( c, List b ) -> ( c, List b )
        go x ( acc, ys ) =
            case mapFn x acc of
                ( newAcc, y ) ->
                    ( newAcc, y :: ys )
    in
    List.foldl go ( init, [] ) xs
        |> Tuple.mapSecond List.reverse
