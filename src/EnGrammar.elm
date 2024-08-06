module EnGrammar exposing (..)

import Grammar exposing (Form(..), Grammar, Nt(..), Tm(..))


en : Grammar
en =
    { title = "English"
    , rules =
        [ ( Nt "Sentence", [ NtForm (Nt "NounPhrase"), NtForm (Nt "VerbPhrase") ] )
        , ( Nt "NounPhrase"
          , [ NtForm (Nt "Determiner"), NtForm (Nt "AdjectivalNoun") ]
          )
        , ( Nt "Determiner", [ TmForm (Tm "the") ] )
        , ( Nt "Determiner", [ TmForm (Tm "a") ] )
        , ( Nt "Determiner", [ TmForm (Tm "every") ] )
        , ( Nt "AdjectivalNoun", [ NtForm (Nt "Adjective"), NtForm (Nt "Noun") ] )
        , ( Nt "AdjectivalNoun", [ NtForm (Nt "Noun") ] )
        , ( Nt "Adjective", [ TmForm (Tm "big") ] )
        , ( Nt "Adjective", [ TmForm (Tm "cute") ] )
        , ( Nt "Adjective", [ TmForm (Tm "red") ] )
        , ( Nt "Noun", [ TmForm (Tm "dog") ] )
        , ( Nt "Noun", [ TmForm (Tm "cat") ] )
        , ( Nt "Noun", [ TmForm (Tm "bird") ] )
        , ( Nt "VerbPhrase", [ NtForm (Nt "VerbIntransitive") ] )
        , ( Nt "VerbPhrase", [ NtForm (Nt "VerbTransitive"), NtForm (Nt "NounPhrase") ] )
        , ( Nt "VerbPhrase", [ NtForm (Nt "Copula"), NtForm (Nt "Adjective") ] )
        , ( Nt "VerbIntransitive", [ TmForm (Tm "sleeps") ] )
        , ( Nt "VerbTransitive", [ TmForm (Tm "sees") ] )
        , ( Nt "VerbTransitive", [ TmForm (Tm "likes") ] )
        , ( Nt "Copula", [ TmForm (Tm "was") ] )
        , ( Nt "Copula", [ TmForm (Tm "seemed") ] )
        , ( Nt "Copula", [ TmForm (Tm "felt") ] )
        ]
    }
