module FormattedText.Fuzz exposing (Markup(..), customFormattedText, formattedText, markup)

{-| Fuzzers of FormattedText types

This can be used to test code that makes use of FormattedText.

-}

import FormattedText exposing (FormattedText, Range)
import Fuzz exposing (..)


{-| A arbitrary Markup type used by the formattedText fuzzer.

If this isn't the right markup type for you, use `customFormattedText`.

-}
type Markup
    = Red
    | Blue
    | Green


{-| Fuzzer of FormattedText types.
-}
formattedText : Fuzzer (FormattedText Markup)
formattedText =
    customFormattedText string markup


{-| Customizable fuzzer of FormattedText types.

You can use your own text and markup fuzzers or use the standard `string` fuzzer
and the markup fuzzer provided by this module.

-}
customFormattedText : Fuzzer String -> Fuzzer markup -> Fuzzer (FormattedText markup)
customFormattedText textFuzzer markupFuzzer =
    map2 buildFormattedText textFuzzer (shortList (protoRange markupFuzzer))


{-| Fuzzer of FormattedText markups.

Because any type can be used as a markup this is just a suggestion provided for the lazy developer ;).

-}
markup : Fuzzer Markup
markup =
    oneOf [ constant Red, constant Blue, constant Green ]


{-| When we generate ranges with random start and end values,
it becomes unlikely for short texts to have ranges only partially covering them.

This strategy will result on the same mix of partially and completely covering ranges
regardless of the text length.

-}
protoRange : Fuzzer markup -> Fuzzer (String -> Range markup)
protoRange markupFuzzer =
    let
        coverCompletely : markup -> String -> Range markup
        coverCompletely markup text =
            { tag = markup
            , start = 0
            , end = String.length text
            }

        coverPartially : markup -> Int -> Int -> String -> Range markup
        coverPartially markup n m text =
            let
                ln =
                    String.length text

                start =
                    safeMod n ln

                end =
                    n + safeMod m (ln - start)
            in
            { tag = markup
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
        [ ( 1, map coverCompletely markupFuzzer )
        , ( 5, map3 coverPartially markupFuzzer int int )
        ]


buildFormattedText : String -> List (String -> Range markup) -> FormattedText markup
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