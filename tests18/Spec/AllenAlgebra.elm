module Spec.AllenAlgebra exposing (..)

import AllenAlgebra
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)


suite : Test
suite =
    describe "AllenAlgebra"
        [ fuzz2 Fuzz.int Fuzz.int "An Allen Algebra compacts into an Order when its ranges are points." <|
            \x y ->
                case compare x y of
                    LT ->
                        Expect.equal AllenAlgebra.Before (AllenAlgebra.relation { start = x, end = x } { start = y, end = y })

                    EQ ->
                        Expect.equal AllenAlgebra.Equal (AllenAlgebra.relation { start = x, end = x } { start = y, end = y })

                    GT ->
                        Expect.equal AllenAlgebra.After (AllenAlgebra.relation { start = x, end = x } { start = y, end = y })
        ]
