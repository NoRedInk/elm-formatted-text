module Parser exposing (Bite, Parser, parse)

{-| Not really a proper parsing library, but nevertheless a useful abstraction
for the implementation of FormattedText.Tree.
-}


type alias Parser output =
    { bites : List (Bite output)
    , digestRemainder : String -> output
    }


type alias Bite output =
    { chars : Int
    , digest : String -> output
    }


parse : String -> Parser output -> List output
parse text parser =
    List.foldl takeBite ( [], text ) parser.bites
        |> (\( result, remainder ) ->
                if remainder == "" then
                    result
                else
                    parser.digestRemainder remainder :: result
           )
        |> List.reverse


takeBite : Bite output -> ( List output, String ) -> ( List output, String )
takeBite { chars, digest } ( output, text ) =
    ( digest (String.left chars text) :: output
    , String.dropLeft chars text
    )
