module FormattedText.Fuzz exposing (Tag(..), customFormattedText, formattedText, tag)

import FormattedText exposing (FormattedText, Range)
import Fuzz exposing (..)


type Tag
    = Red
    | Blue
    | Green


{-| Fuzzer of FormattedText types.
-}
formattedText : Fuzzer (FormattedText Tag)
formattedText =
    customFormattedText string tag


{-| Customizable fuzzer of FormattedText types.

You can use your own text and tag fuzzers or use the standard `string` fuzzer
and the tag fuzzer provided by this module.

-}
customFormattedText : Fuzzer String -> Fuzzer tag -> Fuzzer (FormattedText tag)
customFormattedText textFuzzer tagFuzzer =
    map2 buildFormattedText textFuzzer (shortList (protoRange tagFuzzer))


{-| Fuzzer of FormattedText tags.

Because any type can be used as a tag this is just a suggestion provided for the lazy developer ;).

-}
tag : Fuzzer Tag
tag =
    oneOf [ constant Red, constant Blue, constant Green ]


{-| When we generate ranges with random start and end values,
it becomes unlikely for short texts to have ranges only partially covering them.

This strategy will result on the same mix of partially and completely covering ranges
regardless of the text length.

-}
protoRange : Fuzzer tag -> Fuzzer (String -> Range tag)
protoRange tagFuzzer =
    let
        coverCompletely : tag -> String -> Range tag
        coverCompletely tag text =
            { tag = tag
            , start = 0
            , end = String.length text
            }

        coverPartially : tag -> Int -> Int -> String -> Range tag
        coverPartially tag n m text =
            let
                ln =
                    String.length text

                start =
                    safeMod n ln

                end =
                    n + safeMod m (ln - start)
            in
            { tag = tag
            , start = start
            , end = end
            }

        safeMod : Int -> Int -> Int
        safeMod n m =
            if m == 0 then
                0
            else
                n % m
    in
    frequency
        [ ( 1, map coverCompletely tagFuzzer )
        , ( 5, map3 coverPartially tagFuzzer int int )
        ]


buildFormattedText : String -> List (String -> Range tag) -> FormattedText tag
buildFormattedText text protoRanges =
    let
        ranges =
            List.map ((|>) text) protoRanges
    in
    FormattedText.formattedText text ranges


shortList : Fuzzer a -> Fuzzer (List a)
shortList fuzzer =
    Fuzz.oneOf
        [ Fuzz.constant []
        , Fuzz.map (\a -> [ a ]) fuzzer
        , Fuzz.map2 (\a b -> [ a, b ]) fuzzer fuzzer
        , Fuzz.map3 (\a b c -> [ a, b, c ]) fuzzer fuzzer fuzzer
        ]
