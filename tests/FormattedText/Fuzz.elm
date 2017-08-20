module FormattedText.Fuzz exposing (Tag, formattedText, tag)

import FormattedText exposing (FormattedText, Range)
import Fuzz exposing (..)


type alias Tag =
    Int


tag : Fuzzer Tag
tag =
    intRange 0 3


range : Fuzzer tag -> Fuzzer (Range tag)
range tagFuzzer =
    map3 FormattedText.Range tagFuzzer (intRange 0 1000) (intRange 0 1000)


formattedText : Fuzzer tag -> Fuzzer (FormattedText tag)
formattedText tagFuzzer =
    map2 FormattedText.formattedText string (shortList (range tagFuzzer))


shortList : Fuzzer a -> Fuzzer (List a)
shortList fuzzer =
    Fuzz.oneOf
        [ Fuzz.constant []
        , Fuzz.map (\a -> [ a ]) fuzzer
        , Fuzz.map2 (\a b -> [ a, b ]) fuzzer fuzzer
        , Fuzz.map3 (\a b c -> [ a, b, c ]) fuzzer fuzzer fuzzer
        ]
