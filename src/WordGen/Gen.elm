module WordGen.Gen exposing (..)

{-| Ideas taken almost entirely from [_Generating naming languages_](https://mewo2.com/notes/naming-language/)
by Martin O'Leary.
-}

import Html exposing (Html, div, li, text, ul)
import Html.Attributes exposing (class)
import Random as R
import Random.Extra as RX
import Set


allConsonants =
    englishUnfriendlyConsonants
        ++ List.concat (List.repeat englishFriendlyConsonantWeight englishFriendlyConsonants)


englishUnfriendlyConsonants =
    [ 'x' -- Pronounced like the "ch" in "Bach"
    , 'ɣ' -- Pronounced like the "g" in "amigo" (some dialects).
    , 'q' -- Pronounced like "k" but further back in the throat
    , 'ʔ' -- Glottal stop, like the sound between the syllables in "uh-oh"
    ]


englishFriendlyConsonants =
    [ 'p' -- Pronounced like "p" as in "pat"
    , 'b' -- Pronounced like "b" as in "bat"
    , 'f' -- Pronounced like "f" as in "fog"
    , 'v' -- Pronounced like "v" as in "vase"
    , 't' -- Pronounced like "t" as in "top"
    , 'd' -- Pronounced like "d" as in "dog"
    , 'k' -- Pronounced like "k" as in "kite"
    , 'g' -- Pronounced like "g" as in "goat"
    , 'm' -- Pronounced like "m" as in "mop"
    , 'n' -- Pronounced like "n" as in "no"
    , 'θ' -- Pronounced like "th" as in "thin"
    , 'ð' -- Pronounced like "th" as in "this"
    , 'ŋ' -- Pronounced like "ng" as in "sing"
    ]


englishFriendlyConsonantWeight =
    7


allVowels =
    String.toList
        "aaeeiioouu"
        -- Capitalized vowels stand for language-specific vowels. Will be replaced
        -- in the orthography stage.
        ++ [ 'A', 'E', 'I' ]


{-| Sibilants (from Latin: sībilāns : 'hissing') are fricative consonants of
higher amplitude and pitch, made by directing a stream of air with the tongue
towards the teeth.[1] Examples of sibilants are the consonants at the beginning
of the English words sip, zip, ship, and genre. The symbols in the International
Phonetic Alphabet used to denote the sibilant sounds in these words are,
respectively, [s][z] [ʃ][ʒ]. Sibilants have a characteristically intense sound,
which accounts for their paralinguistic use in getting one's attention (e.g.
calling someone using "psst!" or quieting someone using "shhhh!").

In the alveolar hissing sibilants [s] and [z], the back of the tongue forms a
narrow channel (is grooved) to focus the stream of air more intensely, resulting
in a high pitch. With the hushing sibilants (occasionally termed shibilants),
such as English [ʃ], [tʃ], [ʒ], and [dʒ], the tongue is flatter, and the
resulting pitch lower.[2][3]

A broader category is stridents, which include more fricatives than sibilants
such as uvulars. Sibilants are a higher pitched subset of the stridents. The
English sibilants are:

    /s, z, ʃ, ʒ, tʃ, dʒ/

while the English stridents are:

    /s, z, ʃ, ʒ, tʃ, dʒ, f, v/

as /f/ and /v/ are stridents but not sibilants because they are lower in pitch.

Be aware, some linguistics use the terms stridents and sibilants interchangeably
to refer to the greater amplitude and pitch compared to other fricatives.[6]

"Stridency" refers to the perceptual intensity of the sound of a sibilant
consonant, or obstacle fricatives or affricates, which refers to the critical
role of the teeth in producing the sound as an obstacle to the airstream.
Non-sibilant fricatives and affricates produce their characteristic sound
directly with the tongue or lips etc. and the place of contact in the mouth,
without secondary involvement of the teeth.[citation needed]

The characteristic intensity of sibilants means that small variations in tongue
shape and position are perceivable, with the result that there are many sibilant
types that contrast in various languages.

(source: [Wikipedia](https://en.wikipedia.org/wiki/Sibilant))

-}
allSibilants =
    [ 's', 'ʃ', 'z', 'ʒ' ]


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
allLiquidsAndApproximants =
    [ 'l' -- Pronounced like "l" as in "lip"
    , 'ɹ' -- Pronounced like "r" as in English "rat"
    , 'r' -- Pronounced like "rr" as in Spanish "perro"
    , 'ɾ' -- Pronounced like "r" as in Spanish "pero"
    , 'ʁ' -- Pronounced like the French "r" in "rouge"
    , 'w' -- Pronounced like "w" as in "wet"
    , 'j' -- Pronounced like "y" as in "yes"
    ]


finalSets =
    [ [ 'm', 'n' ]
    , [ 's', 'k' ]
    , [ 'c', 'ʃ' ]
    , [ 'ð', 'θ' ]
    , [ 'm', 'n', 'ŋ' ]
    , [ 's', 'ʃ', 'z', 'ʒ' ]
    ]


type LetterClass
    = C -- Consonant
    | V -- Vowel
    | S -- Sibilant
    | L -- Liquid/Approximant
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
    , [ C, Opt S, V, C ]
    , [ C, Opt S, V, Opt C ]
    , [ Opt C, S, V, F ]
    , [ Opt S, C, V, C ]
    , [ Opt S, C, V, Opt C ]
    , [ S, Opt C, V, C ]
    , [ S, Opt C, V, F ]
    , [ C, Opt C, V, Opt C ]
    , [ Opt S, Opt C, V, L, Opt C ]
    , [ Opt C, V, F ]
    , [ Opt C, V, Opt C ]
    , [ Opt C, V, Opt C, S ]
    , [ Opt L, V, Opt F ]
    , [ Opt L, V, C, Opt S ]
    , [ Opt C, V, Opt F ]
    , [ Opt C, Opt L, V, C ]
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
    , syllableTemplate : List LetterClass
    }


defaultLanguage : Language
defaultLanguage =
    { consonants = allConsonants
    , vowels = allVowels
    , sibilants = allSibilants
    , liquids = allLiquidsAndApproximants
    , finals = List.concat finalSets
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
    in
    case syll of
        [] ->
            False

        curr :: rest ->
            False
                || cluster [ 's', 'ʃ', 'v' ] [ 's', 'ʃ' ] ( prev, curr )
                || cluster [ 'z', 'ʒ', 'f' ] [ 'z', 'ʒ' ] ( prev, curr )
                || cluster allLiquidsAndApproximants allLiquidsAndApproximants ( prev, curr )
                || cluster [ 't', 'd' ] [ 't', 'd' ] ( prev, curr )
                || cluster [ 'ɣ' ] (allSibilants ++ allConsonants) ( prev, curr )
                || cluster (allSibilants ++ allConsonants) [ 'ɣ' ] ( prev, curr )
                || hasHardClusters lang curr rest


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
            ++ ifNonEmptyList lang.liquids
                (div [] [ text "Liquids/Approximants: ", text (spacedChars lang.liquids) ])
            ++ ifNonEmptyList lang.finals
                (div [] [ text "Finals: ", text (spacedChars lang.finals) ])
            ++ [ div [] [ text "Vowels: ", text (spacedChars lang.vowels) ]
               , div [] [ text "Consonants: ", text (spacedChars lang.consonants) ]
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
        syllableTemplateR : R.Generator (List LetterClass)
        syllableTemplateR =
            RX.choice [] syllableStructureTemplates

        consonantsR =
            RX.subsetMinMax 4 (List.length (englishUnfriendlyConsonants ++ englishFriendlyConsonants) // 5 * 4) allConsonants
                |> R.map (Set.fromList >> Set.toList)

        vowelsR =
            RX.lowerWeightedRange (\x -> sqrt x) 2 (List.length allVowels // 5 * 4)
                |> R.andThen (\n -> RX.subsetN n allVowels)
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
                        RX.subsetMin 1 allSibilants
                            |> ifClassIsRelevant S

                    liquidsR =
                        RX.subsetMin 1 allLiquidsAndApproximants
                            |> ifClassIsRelevant L

                    finalsR =
                        RX.subsetMinMax 1 2 finalSets
                            |> R.map (List.concat >> Set.fromList >> Set.toList)
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
