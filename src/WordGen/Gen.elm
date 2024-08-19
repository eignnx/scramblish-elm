module WordGen.Gen exposing (..)

{-| Ideas taken almost entirely from [_Generating naming languages_](https://mewo2.com/notes/naming-language/)
by Martin O'Leary.
-}

import Html exposing (Html, div, li, text, ul)
import Html.Attributes exposing (class)
import Random as R
import Random.Extra as RX


allConsonants =
    [ 'x'
    , 'ɣ'
    , 'q'
    , 'ʔ'
    , 't'
    , 'd'
    , 'p'
    , 'b'
    , 'f'
    , 'v'
    , 'θ'
    , 'ð'
    , 'k'
    , 'g'
    , 'm'
    , 'n'
    , 'ŋ'
    ]


allVowels =
    String.toList "aeiou"


{-| Sibilants (from Latin: sībilāns : 'hissing') are fricative consonants of higher amplitude and pitch, made by directing a stream of air with the tongue towards the teeth.[1] Examples of sibilants are the consonants at the beginning of the English words sip, zip, ship, and genre. The symbols in the International Phonetic Alphabet used to denote the sibilant sounds in these words are, respectively, [s][z] [ʃ][ʒ]. Sibilants have a characteristically intense sound, which accounts for their paralinguistic use in getting one's attention (e.g. calling someone using "psst!" or quieting someone using "shhhh!").

In the alveolar hissing sibilants [s] and [z], the back of the tongue forms a narrow channel (is grooved) to focus the stream of air more intensely, resulting in a high pitch. With the hushing sibilants (occasionally termed shibilants), such as English [ʃ], [tʃ], [ʒ], and [dʒ], the tongue is flatter, and the resulting pitch lower.[2][3]

A broader category is stridents, which include more fricatives than sibilants such as uvulars. Sibilants are a higher pitched subset of the stridents. The English sibilants are:

    /s, z, ʃ, ʒ, tʃ, dʒ/

while the English stridents are:

    /s, z, ʃ, ʒ, tʃ, dʒ, f, v/

as /f/ and /v/ are stridents but not sibilants because they are lower in pitch.[4][5]

Be aware, some linguistics use the terms stridents and sibilants interchangeably to refer to the greater amplitude and pitch compared to other fricatives.[6]

"Stridency" refers to the perceptual intensity of the sound of a sibilant consonant, or obstacle fricatives or affricates, which refers to the critical role of the teeth in producing the sound as an obstacle to the airstream. Non-sibilant fricatives and affricates produce their characteristic sound directly with the tongue or lips etc. and the place of contact in the mouth, without secondary involvement of the teeth.[citation needed]

The characteristic intensity of sibilants means that small variations in tongue shape and position are perceivable, with the result that there are many sibilant types that contrast in various languages.

(source: [Wikipedia](https://en.wikipedia.org/wiki/Sibilant))

-}
sibilantSets =
    [ [ 's', 'ʃ', 'f' ]
    , [ 's', 'ʃ' ]
    , [ 's' ]
    ]


{-| In linguistics, a liquid consonant or simply liquid is any of a class of
consonants that consists of rhotics and voiced lateral approximants, which are
also sometimes described as "R-like sounds" and "L-like sounds". The word liquid
seems to be a calque of the Ancient Greek word ὑγρός (hygrós, transl. moist),
initially used by grammarian Dionysius Thrax to describe Greek sonorants.

Liquid consonants are more prone to be part of consonant clusters and of the
syllable nucleus. Their third formants are generally non-predictable based on
the first two formants. Another important feature is their complex articulation,
which makes them a hard consonant class to study with precision and the last
consonants to be produced by children during their phonological development.
They are also more likely to undergo certain types of phonological changes such
as assimilation, dissimilation and metathesis.

Most languages have at least one liquid in their phonemic inventory. English has
two, `/l/` and `/ɹ/`.

(source: [Wikipedia](https://en.wikipedia.org/wiki/Liquid_consonant))

-}
allLiquids =
    [ 'l', 'r', 'ɹ', 'ʁ', 'w', 'j' ]


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
    , liquids = allLiquids
    , finals = List.concat finalSets
    , syllableTemplates = syllableStructureTemplates
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

        L ->
            RX.choice '￼' lang.liquids |> R.map Just

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
    RX.choice [ C, C, C, C, C, C ] lang.syllableTemplates
        |> R.andThen
            (\template ->
                template
                    |> RX.flattenList (choiceFromLetterClass lang)
                    |> R.map (\cs -> List.filterMap identity cs)
                    |> R.map String.fromList
            )


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
    div [ class "wordgen-lang" ]
        ([ div [] [ text "Consonants: ", text (spacedChars lang.consonants) ]
         , div [] [ text "Vowels: ", text (spacedChars lang.vowels) ]
         ]
            ++ ifNonEmptyList lang.sibilants
                (div [] [ text "Sibilants: ", text (spacedChars lang.sibilants) ])
            ++ ifNonEmptyList lang.liquids
                (div [] [ text "Liquids: ", text (spacedChars lang.liquids) ])
            ++ ifNonEmptyList lang.finals
                (div [] [ text "Finals: ", text (spacedChars lang.finals) ])
            ++ [ div []
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
        )


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


randomLanguage : R.Generator Language
randomLanguage =
    let
        syllableTemplatesR : R.Generator (List (List LetterClass))
        syllableTemplatesR =
            RX.lowerWeightedRange (\x -> x * x * x) 1 maxSyllableTemplates
                |> R.andThen
                    (\nTemplates -> RX.subsetN nTemplates syllableStructureTemplates)

        consonantsR =
            RX.subsetMin 4 allConsonants

        vowelsR =
            RX.subsetMin 1 allVowels
    in
    syllableTemplatesR
        |> R.andThen
            (\syllableTemplates ->
                let
                    unwrapOpt : LetterClass -> LetterClass
                    unwrapOpt c =
                        case c of
                            Opt inner ->
                                unwrapOpt inner

                            _ ->
                                c

                    syllableLetterClasses =
                        List.concat syllableTemplates |> List.map unwrapOpt

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
                        RX.choice [] sibilantSets
                            |> ifClassIsRelevant S

                    liquidsR =
                        RX.subsetMin 1 allLiquids
                            |> ifClassIsRelevant L

                    finalsR =
                        RX.choice [] finalSets
                            |> ifClassIsRelevant F
                in
                consonantsR
                    |> RX.mapPair vowelsR
                    |> RX.mapPair sibilantsR
                    |> RX.mapPair liquidsR
                    |> RX.mapPair finalsR
                    |> R.map
                        (\( ( ( ( consonants, vowels ), sibilants ), liquids ), finals ) ->
                            { consonants = consonants
                            , vowels = vowels
                            , sibilants = sibilants
                            , liquids = liquids
                            , finals = finals
                            , syllableTemplates = syllableTemplates
                            }
                        )
            )
