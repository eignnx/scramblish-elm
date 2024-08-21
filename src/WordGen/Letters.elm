module WordGen.Letters exposing (..)

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


allConsonants : Set.Set Char
allConsonants =
    Set.fromList
        (allBaseConsonants ++ allSibilants ++ allApproximants ++ List.concat finalSets)


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
    , [ 'c', 'ʃ' ]
    , [ 'ð', 'θ' ]
    , [ 'm', 'n', 'ŋ' ]
    , [ 's', 'ʃ', 'z', 'ʒ' ]
    ]
