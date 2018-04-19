module AllenAlgebra exposing (..)

{-| The relations defined by an allen algebra.

<https://www.wikiwand.com/en/Allen's_interval_algebra#/Relations>

-}


type AllenRelation
    = Overlaps
    | OverlapsInverse
    | During
    | DuringInverse
    | Meets
    | MeetsInverse
    | Starts
    | StartsInverse
    | Finishes
    | FinishesInverse
    | Before
    | After
    | Equal


relation : { r | start : comparable, end : comparable } -> { p | start : comparable, end : comparable } -> AllenRelation
relation range1 range2 =
    case
        ( compare (lower range1) (lower range2)
        , compare (lower range1) (upper range2)
        , compare (upper range1) (lower range2)
        , compare (upper range1) (upper range2)
        )
    of
        ( _, _, LT, _ ) ->
            Before

        ( _, _, EQ, _ ) ->
            Meets

        ( _, GT, _, _ ) ->
            After

        ( _, EQ, _, _ ) ->
            MeetsInverse

        ( GT, _, _, LT ) ->
            During

        ( EQ, _, _, LT ) ->
            Starts

        ( GT, _, _, EQ ) ->
            Finishes

        ( LT, _, _, GT ) ->
            DuringInverse

        ( EQ, _, _, GT ) ->
            StartsInverse

        ( LT, _, _, EQ ) ->
            FinishesInverse

        ( LT, _, _, LT ) ->
            Overlaps

        ( GT, _, _, GT ) ->
            OverlapsInverse

        ( EQ, _, _, EQ ) ->
            Equal


{-| Return the lower bound of a range. Naming implies this should be 'start',
but lets make sure.
-}
lower : { r | start : comparable, end : comparable } -> comparable
lower { start, end } =
    min start end


{-| Return the upper bound of a range. Naming implies this should be 'start',
but lets make sure.
-}
upper : { r | start : comparable, end : comparable } -> comparable
upper { start, end } =
    max start end
