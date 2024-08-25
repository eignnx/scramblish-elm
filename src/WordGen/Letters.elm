module WordGen.Letters exposing (..)

import Dict exposing (Dict)
import Random
import Random.Extra
import Set


type LetterClass
    = C -- Consonant
    | V -- Vowel
    | S -- Sibilant
    | A -- Approximant
    | F -- Final
    | Opt LetterClass -- An optional letter class


allBaseConsonants =
    []
        ++ englishUnfriendlyConsonants
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
allApproximants =
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
    , [ 'ʃ' ]
    , [ 'ð', 'θ' ]
    , [ 'm', 'n', 'ŋ' ]
    , [ 's', 'ʃ', 'z', 'ʒ' ]
    ]


type LetterArticulation
    = Consonant ConsonantArticulation
    | Vowel { enExample : String }


type alias ConsonantArticulation =
    { manner : Manner
    , place : Place
    , voicing : Voicing
    , englishFriendly : Bool
    }


type Manner
    = Plosive
    | Fricative
    | Affricate
    | Nasal
    | Liquid
    | Approximant ApproximantType
    | Sibilant
    | Trill
    | Tap


type ApproximantType
    = Lateral
    | Rhotic
    | SemiVowel


type Place
    = Bilabial
    | Labiodental
    | Dental
    | Alveolar
    | Postalveolar
    | Retroflex
    | Palatal
    | Velar
    | Uvular
    | Glottal


type Voicing
    = Voiced
    | Voiceless



-- PROPERTIES


charData : Dict Char LetterArticulation
charData =
    Dict.fromList
        [ ( 't', Consonant { voicing = Voiceless, place = Alveolar, manner = Plosive, englishFriendly = True } )
        , ( 'd', Consonant { voicing = Voiced, place = Alveolar, manner = Plosive, englishFriendly = True } )
        , ( 'k', Consonant { voicing = Voiceless, place = Velar, manner = Plosive, englishFriendly = True } )
        , ( 'g', Consonant { voicing = Voiced, place = Velar, manner = Plosive, englishFriendly = True } )
        , ( 'p', Consonant { voicing = Voiceless, place = Bilabial, manner = Plosive, englishFriendly = True } )
        , ( 'b', Consonant { voicing = Voiced, place = Bilabial, manner = Plosive, englishFriendly = True } )
        , ( 'f', Consonant { voicing = Voiceless, place = Labiodental, manner = Fricative, englishFriendly = True } )
        , ( 'v', Consonant { voicing = Voiced, place = Labiodental, manner = Fricative, englishFriendly = True } )
        , ( 'θ', Consonant { voicing = Voiceless, place = Dental, manner = Fricative, englishFriendly = True } )
        , ( 'ð', Consonant { voicing = Voiced, place = Dental, manner = Fricative, englishFriendly = True } )
        , ( 'x', Consonant { voicing = Voiceless, place = Velar, manner = Fricative, englishFriendly = False } )
        , ( 'ɣ', Consonant { voicing = Voiced, place = Velar, manner = Fricative, englishFriendly = False } )
        , ( 's', Consonant { voicing = Voiceless, place = Alveolar, manner = Fricative, englishFriendly = True } )
        , ( 'z', Consonant { voicing = Voiced, place = Alveolar, manner = Fricative, englishFriendly = True } )
        , ( 'ʃ', Consonant { voicing = Voiceless, place = Postalveolar, manner = Fricative, englishFriendly = True } )
        , ( 'ʒ', Consonant { voicing = Voiced, place = Postalveolar, manner = Fricative, englishFriendly = True } )
        , ( 'm', Consonant { voicing = Voiced, place = Bilabial, manner = Nasal, englishFriendly = True } )
        , ( 'n', Consonant { voicing = Voiced, place = Alveolar, manner = Nasal, englishFriendly = True } )
        , ( 'ŋ', Consonant { voicing = Voiced, place = Velar, manner = Nasal, englishFriendly = True } )
        , ( 'l', Consonant { voicing = Voiced, place = Alveolar, manner = Approximant Lateral, englishFriendly = True } )
        , ( 'ɹ', Consonant { voicing = Voiced, place = Alveolar, manner = Approximant Rhotic, englishFriendly = True } )
        , ( 'r', Consonant { voicing = Voiced, place = Alveolar, manner = Trill, englishFriendly = False } )
        , ( 'ɾ', Consonant { voicing = Voiced, place = Alveolar, manner = Tap, englishFriendly = False } )
        , ( 'ʁ', Consonant { voicing = Voiced, place = Uvular, manner = Approximant Rhotic, englishFriendly = False } )
        , ( 'w', Consonant { voicing = Voiced, place = Bilabial, manner = Approximant SemiVowel, englishFriendly = True } )
        , ( 'j', Consonant { voicing = Voiced, place = Palatal, manner = Approximant SemiVowel, englishFriendly = True } )
        , ( 'ʔ', Consonant { voicing = Voiceless, place = Glottal, manner = Plosive, englishFriendly = False } )
        , ( 'i', Vowel { enExample = "[ea]stern" } )
        , ( 'ɪ', Vowel { enExample = "[i]gloo" } )
        , ( 'ʊ', Vowel { enExample = "b[oo]k" } )
        , ( 'u', Vowel { enExample = "b[oo]t" } )
        , ( 'ə', Vowel { enExample = "sof[a]" } )
        , ( 'ɛ', Vowel { enExample = "[e]gg" } )
        , ( 'æ', Vowel { enExample = "c[a]t" } )
        , ( 'ɑ', Vowel { enExample = "f[a]ther" } )
        , ( 'ʌ', Vowel { enExample = "[u]dder" } )
        , ( 'e', Vowel { enExample = "[a]ble" } )
        ]


searchBy : (LetterArticulation -> Bool) -> List Char
searchBy predicate =
    Dict.filter
        (\_ articulation ->
            predicate articulation
        )
        charData
        |> Dict.keys


searchByMannar : Manner -> List Char
searchByMannar manner =
    searchBy
        (\articulation ->
            case articulation of
                Consonant a ->
                    a.manner == manner

                Vowel _ ->
                    False
        )


searchByPlace : Place -> List Char
searchByPlace place =
    searchBy
        (\articulation ->
            case articulation of
                Consonant a ->
                    a.place == place

                Vowel _ ->
                    False
        )


searchByVoicing : Voicing -> List Char
searchByVoicing voicing =
    searchBy
        (\articulation ->
            case articulation of
                Consonant a ->
                    a.voicing == voicing

                Vowel _ ->
                    False
        )


letterSatisfies : (LetterArticulation -> Bool) -> Char -> Bool
letterSatisfies predicate letter =
    case Dict.get letter charData of
        Just articulation ->
            predicate articulation

        Nothing ->
            False


consonantSatisfies : (ConsonantArticulation -> Bool) -> Char -> Bool
consonantSatisfies predicate letter =
    case Dict.get letter charData of
        Just (Consonant articulation) ->
            predicate articulation

        _ ->
            False


isConsonant : Char -> Bool
isConsonant letter =
    consonantSatisfies (\_ -> True) letter


letterHasManner : Manner -> Char -> Bool
letterHasManner manner letter =
    consonantSatisfies
        (\articulation -> articulation.manner == manner)
        letter


letterHasPlace : Place -> Char -> Bool
letterHasPlace place letter =
    consonantSatisfies
        (\articulation -> articulation.place == place)
        letter


letterHasVoicing : Voicing -> Char -> Bool
letterHasVoicing voicing letter =
    consonantSatisfies
        (\articulation -> articulation.voicing == voicing)
        letter


oppositeVoicing : Voicing -> Voicing
oppositeVoicing voicing =
    case voicing of
        Voiced ->
            Voiceless

        Voiceless ->
            Voiced


oppositeVoicingPair : Char -> List Char
oppositeVoicingPair letterX =
    case Dict.get letterX charData of
        Just (Consonant articulationX) ->
            letterX
                :: searchBy
                    (\letterArticulationY ->
                        case letterArticulationY of
                            Consonant articulationY ->
                                True
                                    && (articulationY.voicing == oppositeVoicing articulationX.voicing)
                                    && (articulationY.manner == articulationX.manner)
                                    && (articulationY.place == articulationX.place)

                            Vowel _ ->
                                False
                    )

        _ ->
            []



-- LETTER SETS


allConsonants : List Char
allConsonants =
    searchBy
        (\articulation ->
            case articulation of
                Consonant _ ->
                    True

                Vowel _ ->
                    False
        )


syllabicConsonants : Set.Set Char
syllabicConsonants =
    Set.fromList
        ([]
            ++ searchByMannar (Approximant Rhotic)
            ++ searchByMannar (Approximant Lateral)
            ++ searchByMannar Fricative
            ++ searchByMannar Nasal
        )


allVowels : Set.Set Char
allVowels =
    charData
        |> Dict.toList
        |> List.filterMap
            (\tup ->
                case tup of
                    ( letter, Vowel _ ) ->
                        Just letter

                    _ ->
                        Nothing
            )
        |> Set.fromList



-- GENERATORS


randomConsonants : Random.Generator (List Char)
randomConsonants =
    charData
        |> Dict.toList
        |> List.filterMap
            (\tup ->
                case tup of
                    ( letter, Consonant articulation ) ->
                        Just ( letter, articulation )

                    _ ->
                        Nothing
            )
        |> List.partition (\( _, a ) -> a.englishFriendly)
        |> (\( enFriendly, enUnfriendly ) ->
                []
                    ++ List.concat (List.repeat englishFriendlyConsonantWeight enFriendly)
                    ++ enUnfriendly
           )
        |> List.map Tuple.first
        -- Remove sibilants and approximants.
        |> List.filter (\c -> not (List.member c allSibilants || List.member c allApproximants))
        -- Choose between 2count and 40% of the consonants.
        |> Random.Extra.subsetMinMax 2 (List.length allConsonants // 5 * 2)
        -- Ensure `d` is chosen if `t` is chosen and vice versa.
        |> Random.map (List.concatMap (\c -> oppositeVoicingPair c))
        |> Random.map (Set.fromList >> Set.toList)
        |> Random.map (Debug.log "consonants")
