module Orthography exposing (..)

import Html.Attributes exposing (maxlength)
import Random
import String exposing (toInt)
import Utils


type OrthoDir
    = Ltr
    | Rtl


type alias Orthography =
    { title : String
    , id : String
    , note : String
    , orthoDir : OrthoDir
    , sample : String
    , wordGenerator : String -> Random.Generator String
    }


chooseOrtho : Random.Generator Orthography
chooseOrtho =
    case orthographies of
        [] ->
            Debug.todo "No orthographies to choose from"

        o :: os ->
            Random.uniform o os


orthographies : List Orthography
orthographies =
    [ romanOrthography
    , oldItalicOrthography
    ]


type ConsonantVowelOrthoraphy
    = CvOrtho
        { consonants : List String
        , followingConsonants : List String
        , vowels : List String
        , maxSegments : Int
        }


wordGeneratorFromCvOrtho : ConsonantVowelOrthoraphy -> (String -> Random.Generator String)
wordGeneratorFromCvOrtho (CvOrtho cvOrtho) =
    let
        wordGenerator : String -> Random.Generator String
        wordGenerator word =
            let
                -- wordSeed =
                --     Random.initialSeed (stringHash word)
                maxLen =
                    toFloat cvOrtho.maxSegments

                chosenLength : Random.Generator Int
                chosenLength =
                    Random.float (1.0 ^ 0.333) (maxLen ^ 0.333)
                        |> Random.map (\r -> r * r * r)
                        |> Random.map truncate

                someC =
                    cvOrtho.consonants |> Utils.randomChoice "￼"

                someCFollow =
                    cvOrtho.followingConsonants |> Utils.randomChoice "￼"

                someV =
                    cvOrtho.vowels |> Utils.randomChoice "￼"

                possiblyAppend : Float -> Random.Generator appendable -> appendable -> Random.Generator appendable
                possiblyAppend pct suffix base =
                    Utils.choose pct
                        (\() -> Random.map2 (++) (Random.constant base) suffix)
                        (\() -> Random.constant base)

                loop : Int -> String -> Random.Generator String
                loop segmentsRemaining wordSoFar =
                    if segmentsRemaining <= 0 then
                        Random.constant wordSoFar

                    else
                        wordSoFar
                            |> Utils.sequenceRandom
                                [ possiblyAppend 0.05 someV
                                , possiblyAppend 1.0 someC
                                , possiblyAppend 0.2 someCFollow
                                , possiblyAppend 0.05 someCFollow
                                , possiblyAppend 1.0 someV
                                , possiblyAppend 0.2 someV
                                , possiblyAppend 0.1 someV
                                ]
                            |> Random.andThen (loop (segmentsRemaining - 1))
            in
            chosenLength
                |> Random.andThen
                    (\len ->
                        loop len ""
                            |> Random.andThen (possiblyAppend 0.333 someC)
                    )
    in
    wordGenerator


romanOrthography : Orthography
romanOrthography =
    { title = "Roman Letters"
    , id = "roman"
    , note = "The Roman alphabet. Easiest for new players who speak English."
    , sample = "REXO RENIV CAPORTEOF QEI"
    , orthoDir = Ltr
    , wordGenerator =
        let
            consonants =
                String.toList "BCDFGHLMNPQRSTVXZ" |> List.map String.fromChar

            followingConsonants =
                String.toList "CGLPRST" |> List.map String.fromChar

            vowels =
                String.toList "AEIOV" |> List.map String.fromChar
        in
        CvOrtho
            { maxSegments = 5
            , consonants = consonants
            , followingConsonants = followingConsonants
            , vowels = vowels
            }
            |> wordGeneratorFromCvOrtho
    }


oldItalicOrthography : Orthography
oldItalicOrthography =
    { title = "Old Italic"
    , id = "old-italic"
    , note = "A dead script used by the Etruscans and other ancient Italian peoples."
    , sample = "𐌑𐌀𐌁𐌉𐌖𐌒 𐌆𐌖𐌀𐌌𐌛𐌄𐌕 𐌛𐌖𐌄𐌕 𐌖𐌐𐌏𐌖"
    , orthoDir = Ltr
    , wordGenerator =
        let
            consonants =
                String.split " " "𐌁 𐌂 𐌃 𐌅 𐌆 𐌇 𐌈 𐌊 𐌋 𐌌 𐌍 𐌎 𐌐 𐌑 𐌒 𐌛 𐌔 𐌕 𐌗 𐌘 𐌙 𐌚"

            vowels =
                String.split " " "𐌀 𐌄 𐌉 𐌏 𐌖"
        in
        CvOrtho
            { maxSegments = 5
            , consonants = consonants
            , followingConsonants = consonants
            , vowels = vowels
            }
            |> wordGeneratorFromCvOrtho
    }
