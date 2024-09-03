module WordStats exposing (..)

import Dict
import Grammar exposing (Lang(..), SyntaxTree)
import List.Extra


type alias WordStats =
    { engCounts : Dict.Dict String Int
    , scrCounts : Dict.Dict String Int
    , hoveredWord : Maybe ( Lang, String )
    , selectedWord : Maybe ( Lang, String )
    , userTranslations : List { eng : String, scr : String }
    }


default : WordStats
default =
    { engCounts = Dict.empty
    , scrCounts = Dict.empty
    , hoveredWord = Nothing
    , selectedWord = Nothing
    , userTranslations =
        -- [ { eng = "asdf", scr = "qwer" }
        -- , { eng = "a2sdf", scr = "q9wer" }
        -- , { eng = "fg8h", scr = "q9wer" }
        -- , { eng = "cvonhasd", scr = "asfb" }
        -- , { eng = "a2sdf", scr = "gbszxc" }
        -- , { eng = "a2sdf", scr = "q9wer" }
        -- , { eng = "sadfads", scr = "asdfnbcv" }
        -- , { eng = "tdfjsk", scr = "dfadlk" }
        -- , { eng = "aodfb", scr = "cvbnak" }
        -- ]
        []
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


getCountForWord : WordStats -> Lang -> String -> Int
getCountForWord stats lang word =
    Maybe.withDefault 0 <|
        Dict.get word <|
            case lang of
                English ->
                    stats.engCounts

                Scramblish ->
                    stats.scrCounts
