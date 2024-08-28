module WordStats exposing (..)

import Dict
import Grammar exposing (SyntaxTree)
import List.Extra


type alias WordStats =
    { counts : Dict.Dict String Int
    , hoveredWord : Maybe String
    , selectedWord : Maybe String
    , userTranslations : List { eng : String, scr : String }
    }


default : WordStats
default =
    { counts = Dict.empty
    , hoveredWord = Nothing
    , selectedWord = Nothing
    , userTranslations = []
    }


countWords : List SyntaxTree -> Dict.Dict String Int
countWords trees =
    List.foldl
        (\tree countsAcc ->
            tree
                |> Grammar.syntaxTreeToWordList
                |> List.Extra.gatherEquals
                |> List.map (\( word, repeats ) -> ( word, List.length repeats + 1 ))
                |> Dict.fromList
                |> mergeCounts countsAcc
        )
        Dict.empty
        trees


mergeCounts : Dict.Dict String Int -> Dict.Dict String Int -> Dict.Dict String Int
mergeCounts counts1 counts2 =
    Dict.foldl
        (\word count acc ->
            Dict.update word
                (\maybeCount ->
                    case maybeCount of
                        Just count2 ->
                            Just (count + count2)

                        Nothing ->
                            Just count
                )
                acc
        )
        counts1
        counts2


getCountForWord : WordStats -> String -> Int
getCountForWord stats word =
    Dict.get word stats.counts |> Maybe.withDefault 0
