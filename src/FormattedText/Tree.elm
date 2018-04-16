module FormattedText.Tree exposing (..)

import AllenAlgebra
import FormattedText.Internal as Internal exposing (FormattedText)


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


trees : (String -> tree) -> (markup -> List tree -> tree) -> FormattedText markup -> List tree
trees leaf node formatted =
    Debug.crash "TODO"


rangeForest : FormattedText markup -> Forest (Range markup)
rangeForest formatted =
    Internal.ranges formatted
        |> List.map (\range -> Tree range [])
        |> List.foldl addTree []
