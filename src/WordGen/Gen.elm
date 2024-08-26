module WordGen.Gen exposing (..)

{-| Ideas taken almost entirely from [_Generating naming languages_](https://mewo2.com/notes/naming-language/)
by Martin O'Leary.
-}

import Html exposing (Html, div, figcaption, figure, li, span, text, ul)
import Html.Attributes exposing (class)
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Random as R
import Random.Extra as RX
import Set
import Utils
import WordGen.Letters as L exposing (LetterClass(..))
import WordGen.Syllable as Syllable exposing (Language)


syllableStructureTemplates : List (List LetterClass)
syllableStructureTemplates =
    [ [ C, V, C ]
    , [ C, V, Opt V, C ]
    , [ C, V, V, Opt C ]
    , [ C, V, Opt C ]
    , [ C, V ]
    , [ V, C ]
    , [ C, V, F ]
    , [ Opt C, V, C ]
    , [ C, V, Opt F ]
    , [ C, Opt A, V, C ]
    , [ C, Opt A, V, F ]
    , [ C, Opt S, V, C ]
    , [ C, Opt S, V, Opt C ]
    , [ Opt C, S, V, F ]
    , [ Opt S, C, V, C ]
    , [ Opt S, C, V, Opt C ]
    , [ S, Opt C, V, C ]
    , [ S, Opt C, V, F ]
    , [ C, Opt C, V, Opt C ]
    , [ Opt S, Opt C, V, A, Opt C ]
    , [ Opt C, V, F ]
    , [ Opt C, V, Opt C ]
    , [ Opt C, V, Opt C, S ]
    , [ Opt A, V, Opt F ]
    , [ Opt A, V, C, Opt S ]
    , [ Opt C, V, Opt F ]
    , [ Opt C, Opt A, V, C ]
    , [ C, V, Opt A, Opt C ]
    , [ Opt C, V, Opt A, C ]
    , [ Opt C, V, A, Opt C ]
    ]


maxSyllableTemplates : Int
maxSyllableTemplates =
    3


defaultLanguage : Language
defaultLanguage =
    { consonants = L.allBaseConsonants
    , vowels = L.allVowels |> Set.toList
    , sibilants = L.allSibilants
    , approximants = L.allApproximants
    , finals = List.concat L.finalSets
    , syllableTemplate = Syllable.defaultSyllableTemplate
    , syllabicConsonantLikelihood = 0.5
    }


invalidSyllable : Language -> String -> Bool
invalidSyllable lang syll =
    syll
        |> String.toList
        |> (\letters ->
                False
                    || hasDuplicateAdjacentLetters lang '④' letters
                    || hasHardClusters lang '⑤' letters
           )


hasDuplicateAdjacentLetters : Language -> Char -> List Char -> Bool
hasDuplicateAdjacentLetters lang prev syll =
    case syll of
        [] ->
            False

        c :: rest ->
            if c == prev then
                True

            else
                hasDuplicateAdjacentLetters lang c rest


hasHardClusters : Language -> Char -> List Char -> Bool
hasHardClusters lang prev syll =
    let
        cluster : List Char -> List Char -> ( Char, Char ) -> Bool
        cluster prevOptions currOptions ( prevCh, currCh ) =
            List.member prevCh prevOptions && List.member currCh currOptions

        disallowedClusters : List ( List Char, List Char ) -> ( Char, Char ) -> Bool
        disallowedClusters clusters ( prevCh, currCh ) =
            case clusters of
                [] ->
                    False

                ( prevOptions, currOptions ) :: rest ->
                    cluster prevOptions currOptions ( prevCh, currCh )
                        || disallowedClusters rest ( prevCh, currCh )
    in
    case syll of
        [] ->
            False

        curr :: rest ->
            (( prev, curr )
                |> disallowedClusters
                    [ ( [ 's', 'ʃ', 'v' ], [ 's', 'ʃ', 'ʒ' ] )
                    , ( [ 'z', 'ʒ', 'f' ], [ 'z', 'ʒ' ] )
                    , ( [ 'd' ], [ 'ʃ' ] )
                    , ( L.allApproximants, L.allApproximants )
                    , ( [ 't', 'd' ], [ 't', 'd' ] )
                    , ( [ 'ð' ], [ 'ʃ' ] )
                    , ( [ 'ɣ' ], L.allConsonants )
                    , ( L.allConsonants, [ 'ɣ' ] )
                    ]
            )
                || hasHardClusters lang curr rest


viewLanguage : Language -> Html msg
viewLanguage lang =
    let
        spacedChars list =
            list |> List.intersperse ' ' |> String.fromList

        ifNonEmptyList : List a -> b -> List b
        ifNonEmptyList list item =
            if List.length list > 0 then
                [ item ]

            else
                []
    in
    figure []
        [ div [ class "wordgen-lang" ]
            ([ div []
                [ text "Syllable Template: "
                , lang.syllableTemplate
                    |> Syllable.viewSyllableTemplate
                    |> text
                ]
             ]
                ++ ifNonEmptyList lang.sibilants
                    (div [] [ text "Sibilants: ", text (spacedChars lang.sibilants) ])
                ++ ifNonEmptyList lang.approximants
                    (div [] [ text "Approximants: ", text (spacedChars lang.approximants) ])
                ++ ifNonEmptyList lang.finals
                    (div [] [ text "Finals: ", text (spacedChars lang.finals) ])
                ++ [ div [] [ text "Vowels: ", text (spacedChars lang.vowels) ]
                   , div [] [ text "Consonants: ", text (spacedChars lang.consonants) ]
                   , div []
                        [ text "Syllabic Consonant Likelihood: "
                        , text
                            ((lang.syllabicConsonantLikelihood * 100 |> round |> String.fromInt) ++ "%")
                        ]
                   ]
            )
        , figcaption [] [ text "C = Consonant, V = Vowel, S = Sibilant, A = Approximant, F = Final" ]
        ]


randomLanguage : R.Generator Language
randomLanguage =
    let
        syllableTemplateR : R.Generator Syllable.SyllableTemplate
        syllableTemplateR =
            Syllable.randomSyllableTemplate

        consonantsR =
            L.randomConsonants

        vowelsR =
            RX.lowerWeightedRange (\x -> sqrt x) 2 (Set.size L.allVowels // 5 * 4)
                |> R.andThen (\n -> RX.subsetN n (Set.toList L.allVowels))
                |> R.map (Set.fromList >> Set.toList)
    in
    syllableTemplateR
        |> R.andThen
            (\syllableTemplate ->
                let
                    unwrapOpt : LetterClass -> LetterClass
                    unwrapOpt c =
                        case c of
                            Opt inner ->
                                unwrapOpt inner

                            _ ->
                                c

                    syllableLetterClasses : List LetterClass
                    syllableLetterClasses =
                        syllableTemplate |> Syllable.syllableTemplateToLetterClasses |> List.map unwrapOpt

                    ifClassIsRelevant :
                        LetterClass
                        -> R.Generator (List b)
                        -> R.Generator (List b)
                    ifClassIsRelevant class itemsR =
                        if List.member class syllableLetterClasses then
                            itemsR

                        else
                            R.constant []

                    sibilantsR : R.Generator (List Char)
                    sibilantsR =
                        RX.subsetMin 1 L.allSibilants
                            |> ifClassIsRelevant S

                    approximantsR =
                        RX.subsetMin 1 L.allApproximants
                            |> ifClassIsRelevant A

                    finalsR =
                        RX.subsetMinMax 1 2 L.finalSets
                            |> R.map (List.concat >> Set.fromList >> Set.toList)
                            |> ifClassIsRelevant F

                    syllabicConsonantLiklihoodR =
                        R.float 0 1
                            |> R.map (\x -> 0.02 * (1 / (1 - x) - 1))
                            |> R.map (clamp 0 1)
                in
                consonantsR
                    |> RX.mapPair vowelsR
                    |> RX.mapPair sibilantsR
                    |> RX.mapPair approximantsR
                    |> RX.mapPair finalsR
                    |> RX.mapPair syllabicConsonantLiklihoodR
                    |> R.map
                        (\( ( ( ( ( consonants, vowels ), sibilants ), approximants ), finals ), syllabicConsonantLikelihood ) ->
                            { consonants = consonants
                            , vowels = vowels
                            , sibilants = sibilants
                            , approximants = approximants
                            , finals = finals
                            , syllableTemplate = syllableTemplate
                            , syllabicConsonantLikelihood = syllabicConsonantLikelihood
                            }
                        )
            )


randomWord : Syllable.Language -> R.Generator (List Syllable.Syll)
randomWord lang =
    RX.lowerWeightedRange (\x -> x ^ 3) 1 4
        |> R.andThen (\n -> R.list n (Syllable.randomSyllable lang))


viewWord : List Syllable.Syll -> Html msg
viewWord sylls =
    List.intersperse (text ".") (List.map Syllable.viewSyllable sylls)
        |> span []
