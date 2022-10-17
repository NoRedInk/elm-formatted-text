module Spec.Parser exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "Parser"
        [ fuzz2 (parser (Fuzz.constant identity)) Fuzz.string "Parser an entire string" <|
            \parser_ string ->
                Parser.parse string parser_
                    |> String.concat
                    |> Expect.equal string
        ]


parser : Fuzzer (String -> a) -> Fuzzer (Parser.Parser a)
parser digest =
    Fuzz.map2 Parser.Parser (Fuzz.list (bite digest)) digest


bite : Fuzzer (String -> a) -> Fuzzer (Parser.Bite a)
bite digest =
    Fuzz.map2 Parser.Bite (Fuzz.intRange 0 10) digest
