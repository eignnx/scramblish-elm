module WordGen.Syllable exposing (..)

import List.Extra
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Random as R
import Random.Extra as RX
import Utils
import WordGen.Letters as L exposing (LetterClass(..))


type alias Language =
    { syllableTemplate : SyllableTemplate
    , consonants : List Char
    , vowels : List Char
    , sibilants : List Char
    , approximants : List Char
    , finals : List Char
    }


type alias Syllable a =
    { onset : List a
    , nucleus : Nonempty a
    , coda : List a
    }


restrictionChoices : List { name : String, rule : Syllable Char -> Bool }
restrictionChoices =
    [ { name = "Leading NG"
      , rule =
            \syll ->
                case syll.onset of
                    first :: _ ->
                        first == 'ŋ'

                    [] ->
                        False
      }
    , { name = "No H at end of coda"
      , rule = \syll -> List.member 'h' syll.coda
      }
    , { name = "No semivowel in coda"
      , rule = \syll -> Utils.any (L.letterHasManner (L.Approximant L.SemiVowel)) syll.coda
      }

    -- , { name = "" }
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



-- ([]
--     ++ (template.coda |> List.map stringFromLetterClass)
--     ++ (Nonempty.toList template.nucleus |> List.map stringFromLetterClass)
--     ++ (template.onset |> List.map stringFromLetterClass)
-- )
--     |> String.concat


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


randomSyllable : Language -> R.Generator (Syllable Char)
randomSyllable lang =
    let
        template =
            lang.syllableTemplate

        onsetR : R.Generator (List Char)
        onsetR =
            template.onset |> RX.flattenMaybeList (choiceFromLetterClass lang)

        nucleusR : R.Generator (Nonempty Char)
        nucleusR =
            template.nucleus |> RX.flattenMaybeNonempty '￼' (choiceFromLetterClass lang)

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
                        List.all
                            (\{ rule } ->
                                not (rule syll)
                            )
                            restrictionChoices
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


debugSyllableFlag : Bool
debugSyllableFlag =
    True


ifDebugSyllableFlag : a -> a -> a
ifDebugSyllableFlag a b =
    if debugSyllableFlag then
        a

    else
        b


renderSyllable : Syllable Char -> String
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
