module WordGen.Syllable exposing (..)

import List.Nonempty as Nonempty exposing (Nonempty(..))
import Utils
import WordGen.Letters as L


type alias Syllable =
    { onset : List Char
    , nucleus : Nonempty Char
    , coda : List Char
    }


restrictionChoices : List { name : String, rule : Syllable -> Bool }
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
