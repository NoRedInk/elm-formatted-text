module EqualCheck exposing (EqualCheck, listContents, map, result)

import Compare
import Expect exposing (Expectation)


{-| Type signature of a (custom) equality check.
-}
type alias EqualCheck a =
    a -> a -> Expectation


{-| Transform values before doing an equality test.
-}
map : (a -> b) -> EqualCheck b -> EqualCheck a
map fn equalCheck =
    \a b -> equalCheck (fn a) (fn b)


{-| Use a custom equality check on a value wrapped in a result.
-}
result : EqualCheck err -> EqualCheck value -> EqualCheck (Result err value)
result errCheck valueCheck =
    \a b ->
        case ( a, b ) of
            ( Err errA, Err errB ) ->
                errCheck errA errB

            ( Ok valueA, Ok valueB ) ->
                valueCheck valueA valueB

            _ ->
                Expect.equal a b


{-| Check if the contents of two lists are the same (order is not taken into account).
Usually doing this kind of check implies you might want to use Sets instead, but due to
the comparable limitation on their contents this is not always possible.
-}
listContents : Compare.Comparator a -> EqualCheck (List a)
listContents compare =
    map (List.sortWith compare) Expect.equalLists
