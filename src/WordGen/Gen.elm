module WordGen.Gen exposing (..)

{-| Ideas taken almost entirely from [_Generating naming languages_](https://mewo2.com/notes/naming-language/)
by Martin O'Leary.
-}

import Html exposing (Html, div, figcaption, figure, li, text, ul)
import Html.Attributes exposing (class)
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Random as R
import Random.Extra as RX
import Set
import Utils
import WordGen.Letters as L exposing (LetterClass(..))


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


type alias Language =
    { consonants : List Char
    , vowels : List Char
    , sibilants : List Char
    , approximants : List Char
    , finals : List Char
    , syllableTemplate : List L.LetterClass
    }


defaultLanguage : Language
defaultLanguage =
    { consonants = L.allBaseConsonants
    , vowels = L.allVowels
    , sibilants = L.allSibilants
    , approximants = L.allApproximants
    , finals = List.concat L.finalSets
    , syllableTemplate = [ C, V, C ]
    }


choiceFromLetterClass : Language -> LetterClass -> R.Generator (Maybe Char)
choiceFromLetterClass lang class =
    case class of
        C ->
            RX.choice '￼' lang.consonants |> R.map Just

        V ->
            RX.choice '￼' lang.vowels |> R.map Just

        S ->
            RX.choice '￼' lang.sibilants |> R.map Just

        A ->
            RX.choice '￼' lang.approximants |> R.map Just

        F ->
            RX.choice '￼' lang.finals |> R.map Just

        Opt c ->
            RX.chance 0.5
                |> R.andThen
                    (\b ->
                        if b then
                            choiceFromLetterClass lang c

                        else
                            R.constant Nothing
                    )


randomSyllable : Language -> R.Generator String
randomSyllable lang =
    lang.syllableTemplate
        |> RX.flattenList (choiceFromLetterClass lang)
        |> R.map (\cs -> List.filterMap identity cs)
        |> R.map String.fromList
        |> R.andThen
            (\syll ->
                if invalidSyllable lang syll then
                    randomSyllable lang

                else
                    R.constant syll
            )


invalidSyllable : Language -> String -> Bool
invalidSyllable lang syll =
    syll
        |> String.toList
        |> (\letters ->
                False
                    || hasDuplicateAdjacentLetters lang '￼' letters
                    || hasHardClusters lang '￼' letters
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
                    |> List.map stringFromLetterClass
                    |> String.join ""
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
                   ]
            )
        , figcaption [] [ text "C = Consonant, V = Vowel, S = Sibilant, A = Approximant, F = Final" ]
        ]


stringFromLetterClass : LetterClass -> String
stringFromLetterClass c =
    case c of
        C ->
            "C"

        V ->
            "V"

        S ->
            "S"

        A ->
            "A"

        F ->
            "F"

        Opt inner ->
            "[" ++ stringFromLetterClass inner ++ "]"


randomLanguage : R.Generator Language
randomLanguage =
    let
        syllableTemplateR : R.Generator (List LetterClass)
        syllableTemplateR =
            RX.choice [] syllableStructureTemplates

        consonantsR =
            L.randomConsonants

        vowelsR =
            RX.lowerWeightedRange (\x -> sqrt x) 2 (List.length L.allVowels // 5 * 4)
                |> R.andThen (\n -> RX.subsetN n L.allVowels)
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

                    syllableLetterClasses =
                        syllableTemplate |> List.map unwrapOpt

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
                in
                consonantsR
                    |> RX.mapPair vowelsR
                    |> RX.mapPair sibilantsR
                    |> RX.mapPair approximantsR
                    |> RX.mapPair finalsR
                    |> R.map
                        (\( ( ( ( consonants, vowels ), sibilants ), approximants ), finals ) ->
                            { consonants = consonants
                            , vowels = vowels
                            , sibilants = sibilants
                            , approximants = approximants
                            , finals = finals
                            , syllableTemplate = syllableTemplate
                            }
                        )
            )


randomWord : Language -> R.Generator String
randomWord lang =
    R.int 1 4
        |> R.andThen (\n -> R.list n (randomSyllable lang))
        |> R.map (List.intersperse ".")
        |> R.map String.concat
