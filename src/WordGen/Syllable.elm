module WordGen.Syllable exposing (..)

import Dict
import Html as H
import Html.Attributes as HA
import List.Extra
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Random as R
import Random.Extra as RX
import Set
import Utils
import WordGen.Letters as L exposing (LetterClass(..))


type alias Language =
    { syllableTemplate : SyllableTemplate
    , consonants : List Char
    , vowels : List Char
    , sibilants : List Char
    , approximants : List Char
    , finals : List Char
    , syllabicConsonantLikelihood : Float
    }


consonantsOfLang : Language -> Set.Set Char
consonantsOfLang lang =
    (lang.consonants ++ lang.sibilants ++ lang.approximants ++ lang.finals)
        |> Set.fromList


type alias Syllable a =
    { onset : List a
    , nucleus : Nonempty a
    , coda : List a
    }


type alias Syll =
    Syllable Char


type alias SyllableRestriction =
    { name : String
    , importance : RestrictionImportance
    , rule : Language -> Syll -> Bool
    }


type RestrictionImportance
    = Required
    | EnglishLike


restrictionChoices : List SyllableRestriction
restrictionChoices =
    [ { name = "Leading NG"
      , importance = EnglishLike
      , rule =
            \_ syll ->
                case syll.onset of
                    first :: _ ->
                        first == 'ŋ'

                    [] ->
                        False
      }
    , { name = "No H at end of coda"
      , importance = EnglishLike
      , rule = \_ syll -> List.member 'h' syll.coda
      }
    , { name = "No semivowel in coda"
      , importance = EnglishLike
      , rule = \_ syll -> Utils.any (L.letterHasManner (L.Approximant L.SemiVowel)) syll.coda
      }
    , { name = "No duplicate consonants"
      , importance = Required
      , rule =
            \_ syll ->
                String.toList (renderSyllable syll)
                    |> Utils.adjacentPairs
                    |> Utils.any (\( a, b ) -> a == b && L.isConsonant a)
      }
    , { name = "No difficult pairs"
      , importance = EnglishLike
      , rule =
            \lang syll ->
                let
                    rendered : List Char
                    rendered =
                        renderSyllable syll |> String.toList

                    pairRestrictions : List ( List Char, List Char ) -> ( Char, Char ) -> Bool
                    pairRestrictions restrictions ( a, b ) =
                        restrictions
                            |> Utils.any (\( aList, bList ) -> List.member a aList && List.member b bList)
                in
                rendered
                    |> Utils.adjacentPairs
                    |> Utils.any
                        (pairRestrictions
                            [ ( [ 'ŋ' ], lang.consonants )
                            , ( lang.sibilants, lang.sibilants )
                            , ( lang.approximants, lang.approximants )
                            , ( lang.finals, lang.finals )
                            ]
                        )
      }
    ]


type alias SyllableTemplate =
    Syllable L.LetterClass


defaultSyllableTemplate : SyllableTemplate
defaultSyllableTemplate =
    { onset = [], nucleus = Nonempty V [ V, V, V, V ], coda = [] }


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


viewSyllableTemplate : SyllableTemplate -> String
viewSyllableTemplate template =
    List.Extra.interweave
        (ifDebugSyllableFlag
            [ "("
            , ")("
            , ")("
            , ")"
            ]
            []
        )
        [ template.onset |> List.map stringFromLetterClass |> String.concat
        , Nonempty.toList template.nucleus |> List.map stringFromLetterClass |> String.concat
        , template.coda |> List.map stringFromLetterClass |> String.concat
        ]
        |> String.concat


mapSyllable :
    { onset : List a -> List b
    , nucleus : Nonempty a -> Nonempty b
    , coda : List a -> List b
    }
    -> Syllable a
    -> Syllable b
mapSyllable f a =
    { onset = f.onset a.onset
    , nucleus = f.nucleus a.nucleus
    , coda = f.coda a.coda
    }


syllableTemplateToLetterClasses : SyllableTemplate -> List LetterClass
syllableTemplateToLetterClasses template =
    template.onset ++ Nonempty.toList template.nucleus ++ template.coda


randomSyllableTemplate : R.Generator SyllableTemplate
randomSyllableTemplate =
    RX.choice defaultSyllableTemplate
        ([ ( [ C ], Nonempty V [], [] ) -- CV
         , ( [ C ], Nonempty V [], [ C ] ) -- CVC
         , ( [ C ], Nonempty V [], [ Opt C ] ) -- CV[C]
         , ( [ Opt C ], Nonempty V [], [ C ] ) -- [C]VC
         , ( [ Opt C ], Nonempty V [], [ Opt C ] ) -- [C]V[C]
         , ( [ C ], Nonempty V [], [ F ] ) -- CVF
         , ( [ C ], Nonempty V [], [ Opt F ] ) -- CV[F]
         , ( [ C, Opt S ], Nonempty V [], [ C ] ) -- C[S]VC
         , ( [ Opt S ], Nonempty V [ Opt V ], [ A ] ) -- [S]V[V][A]
         , ( [ Opt A ], Nonempty V [], [ C ] ) -- [A]VC
         , ( [ A ], Nonempty V [], [ F ] ) -- AVF
         ]
            |> List.map
                (\( onset, nucleus, coda ) ->
                    { onset = onset
                    , nucleus = nucleus
                    , coda = coda
                    }
                )
        )


randomSyllable : Language -> R.Generator Syll
randomSyllable lang =
    let
        template =
            lang.syllableTemplate

        onsetR : R.Generator (List Char)
        onsetR =
            template.onset |> RX.flattenMaybeList (choiceFromLetterClass lang)

        nucleusR : R.Generator (Nonempty Char)
        nucleusR =
            template.nucleus
                |> RX.flattenMaybeNonempty '⑥'
                    (\class ->
                        case class of
                            V ->
                                let
                                    langSyllabicConsonants =
                                        Debug.log "syllabic consonants"
                                            (Set.toList (Set.intersect L.syllabicConsonants (consonantsOfLang lang)))
                                in
                                R.float 0 1
                                    |> R.andThen
                                        (\pct ->
                                            if pct < lang.syllabicConsonantLikelihood && List.length langSyllabicConsonants > 0 then
                                                -- Allow syllabic consonant in place of vowel
                                                RX.choice '⑦' langSyllabicConsonants

                                            else
                                                RX.choice '⑧' lang.vowels
                                        )
                                    |> R.map Just

                            _ ->
                                choiceFromLetterClass lang class
                    )

        codaR : R.Generator (List Char)
        codaR =
            template.coda |> RX.flattenMaybeList (choiceFromLetterClass lang)
    in
    R.map3
        (\onset nucleus coda -> { onset = onset, nucleus = nucleus, coda = coda })
        onsetR
        nucleusR
        codaR
        |> -- apply restrictions
           R.andThen
            (\syll ->
                let
                    passRestrictions =
                        restrictionChoices |> List.all (\{ rule } -> not (rule lang syll))
                in
                if passRestrictions then
                    R.constant syll

                else
                    randomSyllable lang
            )


choiceFromLetterClass : Language -> LetterClass -> R.Generator (Maybe Char)
choiceFromLetterClass lang class =
    case class of
        C ->
            RX.choice '⑨' lang.consonants |> R.map Just

        V ->
            RX.choice '⑩' lang.vowels |> R.map Just

        S ->
            RX.choice '⑪' lang.sibilants |> R.map Just

        A ->
            RX.choice '⑫' lang.approximants |> R.map Just

        F ->
            RX.choice '⑬' lang.finals |> R.map Just

        Opt c ->
            RX.chance 0.5
                |> R.andThen
                    (\b ->
                        if b then
                            choiceFromLetterClass lang c

                        else
                            R.constant Nothing
                    )


debugSyllableFlag : Bool
debugSyllableFlag =
    -- True
    False


ifDebugSyllableFlag : a -> a -> a
ifDebugSyllableFlag a b =
    if debugSyllableFlag then
        a

    else
        b


renderSyllable : Syll -> String
renderSyllable syll =
    List.Extra.interweave
        (ifDebugSyllableFlag
            [ [ '(' ]
            , [ ')', '(' ]
            , [ ')', '(' ]
            , [ ')' ]
            ]
            []
        )
        [ syll.onset
        , Nonempty.toList syll.nucleus
        , syll.coda
        ]
        |> List.concat
        |> String.fromList


viewSyllable : Syll -> H.Html msg
viewSyllable syll =
    let
        -- render vowels with a `title` of their english example
        renderChar : Char -> H.Html msg
        renderChar c =
            case Dict.get c L.charData of
                Just (L.Vowel { enExample }) ->
                    let
                        title =
                            "'" ++ String.fromChar c ++ "' as in '" ++ enExample ++ "'"
                    in
                    H.span [ HA.title title ] [ H.text (String.fromChar c) ]

                Just _ ->
                    H.text (String.fromChar c)

                Nothing ->
                    H.span [] [ H.text ("⑭(" ++ String.fromChar c ++ ")") ]
    in
    H.span [ HA.class "syllable" ]
        [ H.span [ HA.class "onset" ] (syll.onset |> List.map renderChar)
        , H.span [ HA.class "nucleus" ] (Nonempty.toList syll.nucleus |> List.map renderChar)
        , H.span [ HA.class "coda" ] (syll.coda |> List.map renderChar)
        ]
