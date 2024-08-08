module Orthography exposing (..)

import Html.Attributes exposing (alt)
import Random
import Utils


type OrthoDir
    = Ltr
    | Rtl


type Orthography
    = Ortho
        { title : String
        , note : String
        , orthoDir : OrthoDir
        , sample : String
        , wordGenerator : String -> Random.Generator String
        }


type ConsonantVowelOrthoraphy
    = CvOrtho
        { consonants : List String
        , followingConsonants : List String
        , vowels : List String
        , maxSegments : Int
        }


orthoFromCvOrtho : ConsonantVowelOrthoraphy -> Orthography -> Orthography
orthoFromCvOrtho (CvOrtho cvOrtho) (Ortho baseOrtho) =
    let
        wordGenerator : String -> Random.Generator String
        wordGenerator word =
            let
                -- wordSeed =
                --     Random.initialSeed (stringHash word)
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
            loop cvOrtho.maxSegments ""
                |> Random.andThen (possiblyAppend 0.333 someC)
    in
    Ortho { baseOrtho | wordGenerator = wordGenerator }


romanOrthography : Orthography
romanOrthography =
    orthoFromCvOrtho
        (let
            consonants =
                String.toList "BCDFGHLMNPQRSTVXZ" |> List.map String.fromChar

            followingConsonants =
                String.toList "CGLPRST" |> List.map String.fromChar

            vowels =
                String.toList "AEIOV" |> List.map String.fromChar
         in
         CvOrtho
            { maxSegments = 6
            , consonants = consonants
            , followingConsonants = followingConsonants
            , vowels = vowels
            }
        )
        (Ortho
            { title = "Roman Letters"
            , note = "The Roman alphabet. Easiest for new players who speak English."
            , sample = "REXO RENIV CAPORTEOF QEI"
            , orthoDir = Ltr
            , wordGenerator = \_ -> Random.constant "<OVERRIDE>"
            }
        )


oldItalicOrthography : Orthography
oldItalicOrthography =
    orthoFromCvOrtho
        (let
            consonants =
                String.split " " "𐌁 𐌂 𐌃 𐌅 𐌆 𐌇 𐌈 𐌊 𐌋 𐌌 𐌍 𐌎 𐌐 𐌑 𐌒 𐌛 𐌔 𐌕 𐌗 𐌘 𐌙 𐌚"

            vowels =
                String.split " " "𐌀 𐌄 𐌉 𐌏 𐌖"
         in
         CvOrtho
            { maxSegments = 6
            , consonants = consonants
            , followingConsonants = consonants
            , vowels = vowels
            }
        )
        (Ortho
            { title = "Old Italic"
            , note = "A dead script used by the Etruscans and other ancient Italian peoples."
            , sample = "𐌑𐌀𐌁𐌉𐌖𐌒 𐌆𐌖𐌀𐌌𐌛𐌄𐌕 𐌛𐌖𐌄𐌕 𐌖𐌐𐌏𐌖"
            , orthoDir = Ltr
            , wordGenerator = \_ -> Random.constant "<OVERRIDE>"
            }
        )
