module FormattedText.Internal exposing (FormattedText, addRange, equal, equalRanges, fromString, overlap, ranges, text)

{-| These types and functions are pulled from `Nri.FormattedText` to ensure constraints on the `FormattedText` type are always kept.
-}


{-| The following constraints should hold for the FormattedText type:

  - Ranges of the same tag never overlap.
  - The end position of the range comes after the start position.
  - Both end and start positions fall within the string.

-}
type FormattedText tag
    = FormattedText String (List (Range tag))


type alias Range tag =
    { tag : tag
    , start : Int
    , end : Int
    }


{-| -}
fromString : String -> FormattedText tag
fromString text =
    FormattedText text []


{-| Remove all range returning only the content.
-}
text : FormattedText tag -> String
text (FormattedText text _) =
    text


{-| Only one to add a range to formatted text. The range is added in a way that observes the constraints listed under the type.
-}
addRange : Range tag -> FormattedText tag -> FormattedText tag
addRange { tag, start, end } (FormattedText text ranges) =
    let
        newRange : Range tag
        newRange =
            { tag = tag
            , start = start |> withinString
            , end = end |> atLeast start |> withinString
            }

        withinString : Int -> Int
        withinString =
            atLeast 0 >> atMost (String.length text)

        atLeast : Int -> Int -> Int
        atLeast =
            max

        atMost : Int -> Int -> Int
        atMost =
            min

        empty : Range tag -> Bool
        empty { start, end } =
            start == end

        ( overlapping, nonOverlapping ) =
            List.partition (overlap newRange) ranges

        mergedOverlap =
            { tag = newRange.tag
            , start =
                List.map .start (newRange :: overlapping)
                    |> List.minimum
                    |> Maybe.withDefault newRange.start
            , end =
                List.map .end (newRange :: overlapping)
                    |> List.maximum
                    |> Maybe.withDefault newRange.start
            }
    in
    (mergedOverlap :: nonOverlapping)
        |> List.filter (not << empty)
        |> FormattedText text


{-| Check if two ranges overlap. Exported for testing purposes.
-}
overlap : Range tag -> Range tag -> Bool
overlap a b =
    let
        sameTags =
            a.tag == b.tag

        isOverlapping =
            a.start <= b.end && b.start <= a.end
    in
    sameTags && isOverlapping


{-| -}
ranges : FormattedText tag -> List (Range tag)
ranges (FormattedText _ ranges) =
    ranges


{-| -}
equal : FormattedText markup -> FormattedText markup -> Bool
equal formattedA formattedB =
    let
        textEqual : Bool
        textEqual =
            text formattedA == text formattedB

        rangesEqual : Bool
        rangesEqual =
            equalRanges (ranges formattedA) (ranges formattedB)
    in
    textEqual && rangesEqual


{-| -}
equalRanges : List (Range tag) -> List (Range tag) -> Bool
equalRanges rangesA rangesB =
    let
        order : Range tag -> Range tag -> Order
        order =
            compareRanges rangesA rangesB

        sortRanges : List (Range tag) -> List (Range tag)
        sortRanges ranges =
            List.sortWith order ranges
    in
    sortRanges rangesA == sortRanges rangesB


compareRanges : List (Range tag) -> List (Range tag) -> Range tag -> Range tag -> Order
compareRanges rangesA rangesB =
    let
        tagOrders : List ( Int, tag )
        tagOrders =
            (rangesA ++ rangesB)
                |> List.foldl onlyUnique []
                |> List.indexedMap (,)

        onlyUnique : Range tag -> List tag -> List tag
        onlyUnique range unique =
            if List.member range.tag unique then
                unique
            else
                range.tag :: unique

        getTagOrder : tag -> Int
        getTagOrder searched =
            tagOrders
                |> List.filter (\( index, tag ) -> tag == searched)
                |> List.head
                |> Maybe.map Tuple.first
                -- Doesn't happen
                |> Maybe.withDefault -1

        ordering : Range tag -> Range tag -> Order
        ordering a b =
            if a.start < b.start then
                LT
            else if a.start > b.start then
                GT
            else if a.end < b.end then
                LT
            else if a.end > b.end then
                GT
            else if getTagOrder a.tag < getTagOrder b.tag then
                LT
            else if getTagOrder a.tag > getTagOrder b.tag then
                GT
            else
                EQ
    in
    ordering
