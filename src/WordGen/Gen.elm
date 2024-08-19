module WordGen.Gen exposing (..)

import Html exposing (Html, div, li, text, ul)
import Html.Attributes exposing (class)
import Random
import Random.Extra


allConsonants =
    [ 'ʃ'
    , 'ʒ'
    , 'ʧ'
    , 'ʤ'
    , 'x'
    , 'ɣ'
    , 'q'
    , 'ʔ'
    , 's'
    , 'z'
    , 't'
    , 'd'
    , 'p'
    , 'b'
    , 'f'
    , 'v'
    , 'k'
    , 'g'
    , 'r'
    , 'l'
    , 'w'
    , 'j'
    , 'm'
    , 'n'
    , 'ŋ'
    ]


allVowels =
    String.toList "aeiou"


sibilantSets =
    [ [ 's', 'ʃ', 'f' ]
    , [ 's', 'ʃ' ]
    , [ 's' ]
    ]


liquidSets =
    [ [ 'l', 'r' ]
    , [ 'l' ]
    , [ 'r' ]
    , [ 'w', 'j' ]
    , [ 'l', 'r', 'w', 'j' ]
    ]


finalSets =
    [ [ 'm', 'n' ]
    , [ 's', 'k' ]
    , [ 'm', 'n', 'ŋ' ]
    , [ 's', 'ʃ', 'z', 'ʒ' ]
    ]


type LetterClass
    = C -- Consonant
    | V -- Vowel
    | S -- Sibilant
    | L -- Liquid
    | F -- Final
    | Opt LetterClass -- An optional letter class


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
    , [ C, Opt L, V, C ]
    , [ C, Opt L, V, F ]
    , [ Opt S, C, V, C ]
    , [ Opt S, C, V, F ]
    , [ Opt S, C, V, Opt C ]
    , [ Opt C, V, F ]
    , [ Opt C, V, Opt C ]
    , [ Opt C, V, Opt F ]
    , [ Opt C, Opt L, V, C ]
    , [ V, C ]
    , [ C, V, Opt L, Opt C ]
    , [ Opt C, V, Opt L, C ]
    , [ Opt C, V, L, Opt C ]
    ]


maxSyllableTemplates : Int
maxSyllableTemplates =
    3


type alias Language =
    { consonants : List Char
    , vowels : List Char
    , sibilants : List Char
    , liquids : List Char
    , finals : List Char
    , syllableTemplates : List (List LetterClass)
    }


defaultLanguage : Language
defaultLanguage =
    { consonants = allConsonants
    , vowels = allVowels
    , sibilants = List.concat sibilantSets
    , liquids = List.concat liquidSets
    , finals = List.concat finalSets
    , syllableTemplates = syllableStructureTemplates
    }


choiceFromLetterClass : Language -> LetterClass -> Random.Generator (Maybe Char)
choiceFromLetterClass lang class =
    case class of
        C ->
            Random.Extra.choice '￼' lang.consonants |> Random.map Just

        V ->
            Random.Extra.choice '￼' lang.vowels |> Random.map Just

        S ->
            Random.Extra.choice '￼' lang.sibilants |> Random.map Just

        L ->
            Random.Extra.choice '￼' lang.liquids |> Random.map Just

        F ->
            Random.Extra.choice '￼' lang.finals |> Random.map Just

        Opt c ->
            Random.Extra.chance 0.5
                |> Random.andThen
                    (\b ->
                        if b then
                            choiceFromLetterClass lang c

                        else
                            Random.constant Nothing
                    )


randomSyllable : Language -> Random.Generator String
randomSyllable lang =
    Random.Extra.choice [ C, C, C, C, C, C ] lang.syllableTemplates
        |> Random.andThen
            (\template ->
                template
                    |> Random.Extra.flattenList (choiceFromLetterClass lang)
                    |> Random.map (\cs -> List.filterMap identity cs)
                    |> Random.map String.fromList
            )


viewLanguage : Language -> Html msg
viewLanguage lang =
    let
        spacedChars list =
            list |> List.intersperse ' ' |> String.fromList
    in
    div [ class "wordgen-lang" ]
        [ div [] [ text "Consonants: ", text (spacedChars lang.consonants) ]
        , div [] [ text "Vowels: ", text (spacedChars lang.vowels) ]
        , div [] [ text "Sibilants: ", text (spacedChars lang.sibilants) ]
        , div [] [ text "Liquids: ", text (spacedChars lang.liquids) ]
        , div [] [ text "Finals: ", text (spacedChars lang.finals) ]
        , div []
            [ text "Syllable Templates: "
            , ul []
                (lang.syllableTemplates
                    |> List.map
                        (\template ->
                            template
                                |> List.map stringFromLetterClass
                                |> String.join ""
                                |> text
                                |> (\t -> li [] [ t ])
                        )
                )
            ]
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

        L ->
            "L"

        F ->
            "F"

        Opt inner ->
            "[" ++ stringFromLetterClass inner ++ "]"


randomLanguage : Random.Generator Language
randomLanguage =
    let
        consonantsR =
            Random.Extra.subset allConsonants

        vowelsR =
            Random.Extra.subset allVowels

        sibilantsR =
            Random.Extra.choice [] sibilantSets

        liquidsR =
            Random.Extra.choice [] liquidSets

        finalsR =
            Random.Extra.choice [] finalSets

        syllableTemplatesR =
            Random.float 0 1
                |> Random.andThen
                    (\percent ->
                        let
                            nTemplates =
                                1 + floor (percent * percent * percent * toFloat maxSyllableTemplates)
                        in
                        Random.Extra.subsetN nTemplates syllableStructureTemplates
                    )
    in
    consonantsR
        |> Random.Extra.mapPair vowelsR
        |> Random.Extra.mapPair sibilantsR
        |> Random.Extra.mapPair liquidsR
        |> Random.Extra.mapPair finalsR
        |> Random.Extra.mapPair syllableTemplatesR
        |> Random.map
            (\( ( ( ( ( consonants, vowels ), sibilants ), liquids ), finals ), syllableTemplates ) ->
                { consonants = consonants
                , vowels = vowels
                , sibilants = sibilants
                , liquids = liquids
                , finals = finals
                , syllableTemplates = syllableTemplates
                }
            )
