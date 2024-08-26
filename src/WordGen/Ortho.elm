module WordGen.Ortho exposing (..)

import Random as R
import Random.Extra
import Utils
import WordGen.Letters as L
import WordGen.Phonology exposing (Syll, renderSyllable)


type alias Orthography =
    { title : String
    , letterMapping : MappingFn
    }


type LookaheadAction
    = Drop
    | Keep


type alias MappingFn =
    ( Char, Char ) -> ( List Char, LookaheadAction )


romanOrthoEnglish : Orthography
romanOrthoEnglish =
    { title = "English Orthography in Roman Script"
    , letterMapping =
        \pair ->
            case pair of
                ( 't', 'ʃ' ) ->
                    ( [ 'c', 'h' ], Drop )

                ( 'ʃ', _ ) ->
                    ( [ 's', 'h' ], Keep )

                ( 'd', 'ʒ' ) ->
                    ( [ 'j' ], Drop )

                ( 'ʒ', _ ) ->
                    ( [ 'z', 'h' ], Keep )

                ( 'ɣ', _ ) ->
                    ( [ 'g', 'h' ], Keep )

                ( 'x', _ ) ->
                    ( [ 'k', 'h' ], Keep )

                ( 'j', _ ) ->
                    ( [ 'y' ], Keep )

                ( 'k', 'ɹ' ) ->
                    ( [ 'c', 'h', 'r' ], Drop )

                ( 'ɹ', _ ) ->
                    ( [ 'r' ], Keep )

                ( 'ɾ', _ ) ->
                    ( [ 'r' ], Keep )

                ( 'r', _ ) ->
                    ( [ 'r', 'r' ], Keep )

                ( 'ʁ', _ ) ->
                    ( [ 'r' ], Keep )

                ( 'θ', _ ) ->
                    ( [ 't', 'h' ], Keep )

                ( 'ð', next ) ->
                    if next == '$' || next == '.' then
                        ( [ 't', 'h', 'e' ], Keep )

                    else
                        ( [ 't', 'h' ], Keep )

                ( 'ŋ', _ ) ->
                    ( [ 'n', 'g' ], Keep )

                ( 'i', _ ) ->
                    ( [ 'e', 'e' ], Keep )

                ( 'ɪ', _ ) ->
                    ( [ 'i' ], Keep )

                ( 'ʊ', _ ) ->
                    ( [ 'u' ], Keep )

                ( 'u', _ ) ->
                    ( [ 'o', 'o' ], Keep )

                ( 'ə', next ) ->
                    if L.letterHasVoicing L.Voiced next then
                        ( [ 'u' ], Keep )

                    else
                        ( [ 'e' ], Keep )

                ( 'ɛ', _ ) ->
                    ( [ 'e' ], Keep )

                ( 'æ', _ ) ->
                    ( [ 'a' ], Keep )

                ( 'ɑ', 'ʊ' ) ->
                    ( [ 'a', 'u', 'g', 'h' ], Drop )

                ( 'ɑ', next ) ->
                    if L.isApproximant next || L.letterHasManner L.Nasal next then
                        ( [ 'a' ], Keep )

                    else
                        ( [ 'a', 'h' ], Keep )

                ( 'ʌ', 'f' ) ->
                    ( [ 'o', 'u', 'g', 'h' ], Drop )

                ( 'ʌ', next ) ->
                    if L.isConsonant next then
                        ( [ 'e' ], Keep )

                    else
                        ( [ 'u', 'h' ], Keep )

                ( 'e', next ) ->
                    if next == '$' || next == '.' then
                        ( [ 'a', 'y' ], Keep )

                    else
                        ( [ 'e', 'i' ], Keep )

                ( '.', 'ʔ' ) ->
                    ( [ '-' ], Drop )

                ( 'ʔ', '.' ) ->
                    ( [ '-' ], Keep )

                ( 'ʔ', _ ) ->
                    ( [ '\'' ], Keep )

                ( letter, _ ) ->
                    ( [ letter ], Keep )
    }


romanOrthoFrench : Orthography
romanOrthoFrench =
    { title = "French Orthography in Roman Script"
    , letterMapping =
        \pair ->
            case pair of
                ( 'ʃ', _ ) ->
                    ( [ 'c', 'h' ], Keep )

                ( 'ʒ', _ ) ->
                    ( [ 'j' ], Keep )

                ( 'ɣ', _ ) ->
                    ( [ 'g', 'h' ], Keep )

                ( 'x', _ ) ->
                    ( [ 'x', 'h' ], Keep )

                ( 'j', _ ) ->
                    ( [ 'i', 'l' ], Keep )

                ( 'w', _ ) ->
                    ( [ 'o', 'u' ], Keep )

                ( 'ɹ', _ ) ->
                    ( [ 'r' ], Keep )

                ( 'ɾ', _ ) ->
                    ( [ 'r' ], Keep )

                ( 'ʁ', _ ) ->
                    ( [ 'r' ], Keep )

                ( 'θ', _ ) ->
                    ( [ 's' ], Keep )

                ( 'ð', _ ) ->
                    ( [ 'z' ], Keep )

                ( 'k', _ ) ->
                    ( [ 'c' ], Keep )

                ( 's', next ) ->
                    if List.member next [ 'ɑ', 'æ', 'o', 'u' ] then
                        ( [ 'ç' ], Keep )

                    else
                        ( [ 's' ], Keep )

                ( 'ŋ', _ ) ->
                    ( [ 'n', 'g' ], Keep )

                ( 'o', _ ) ->
                    ( [ 'ô' ], Keep )

                ( 'i', _ ) ->
                    ( [ 'y' ], Keep )

                ( 'ɪ', _ ) ->
                    ( [ 'i' ], Keep )

                ( 'ʊ', _ ) ->
                    ( [ 'œ' ], Keep )

                ( 'u', _ ) ->
                    ( [ 'u' ], Keep )

                ( 'ə', _ ) ->
                    ( [ 'e' ], Keep )

                ( 'ɛ', _ ) ->
                    ( [ 'è' ], Keep )

                ( 'æ', _ ) ->
                    ( [ 'ê' ], Keep )

                ( 'ɑ', _ ) ->
                    ( [ 'â' ], Keep )

                ( 'ʌ', _ ) ->
                    ( [ 'i', 'n' ], Keep )

                ( 'e', _ ) ->
                    ( [ 'é' ], Keep )

                ( 'ʔ', '.' ) ->
                    ( [ '-' ], Keep )

                ( 'ʔ', _ ) ->
                    ( [ '\'' ], Keep )

                ( letter, _ ) ->
                    ( [ letter ], Keep )
    }


orthographies : List Orthography
orthographies =
    [ romanOrthoEnglish
    , romanOrthoFrench
    ]


defaultOrthography : Orthography
defaultOrthography =
    List.head orthographies |> Maybe.withDefault romanOrthoEnglish


randomOrthography : R.Generator Orthography
randomOrthography =
    Random.Extra.choice romanOrthoEnglish orthographies


applyOrthoMappingToWordWithMarkers : Orthography -> List Syll -> String
applyOrthoMappingToWordWithMarkers ortho sylls =
    sylls
        |> List.map renderSyllable
        |> String.join "."
        |> (\word -> "^" ++ word ++ "$")
        |> String.toList
        |> Utils.adjacentPairs
        |> List.map ortho.letterMapping
        |> Debug.log "letterMapping"
        |> List.foldl
            (\( newLetters, newAction ) ( lettersAcc, prevAction ) ->
                case prevAction of
                    Drop ->
                        ( lettersAcc, Keep )

                    Keep ->
                        ( lettersAcc ++ newLetters, newAction )
            )
            ( [], Keep )
        |> Tuple.first
        |> List.filter
            (\letter ->
                True
                    && (letter /= '$')
                    && (letter /= '^')
                    && (letter /= '.')
            )
        |> String.fromList
