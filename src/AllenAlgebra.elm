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


relation : { r | start : Int, end : Int } -> { p | start : Int, end : Int } -> AllenRelation
relation range1 range2 =
    case
        ( compare range1.start range2.start
        , compare range1.start range2.end
        , compare range1.end range2.start
        , compare range1.end range2.end
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
