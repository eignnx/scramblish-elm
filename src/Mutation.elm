module Mutation exposing (..)

import Grammar exposing (Grammar, Nt, SententialForm)
import List.Extra
import Random
import Utils


type alias RuleMut =
    { lhs : Nt
    , clausePermutation : List Int
    , oldSententialForm : SententialForm
    }


ruleMutGenerator : ( Nt, SententialForm ) -> Random.Generator RuleMut
ruleMutGenerator ( nt, sf ) =
    let
        len =
            List.length sf

        indices =
            List.range 0 (len - 1)
    in
    Utils.shuffled indices
        |> Random.map
            (\permutation ->
                { lhs = nt
                , clausePermutation = permutation
                , oldSententialForm = sf
                }
            )


applyRuleMut : RuleMut -> SententialForm
applyRuleMut { clausePermutation, oldSententialForm } =
    List.Extra.zip clausePermutation oldSententialForm
        |> List.sortBy (\( newIndex, _ ) -> newIndex)
        |> List.map (\( _, value ) -> value)


type alias GrammarMut =
    { oldGrammar : Grammar
    , newTitle : String
    , ruleMuts : List RuleMut
    }


grammarMutGenerator : String -> Grammar -> Random.Generator GrammarMut
grammarMutGenerator newTitle grammar =
    grammar.rules
        |> Utils.randomFlattenList ruleMutGenerator
        |> Random.map
            (\ruleMuts ->
                { oldGrammar = grammar
                , newTitle = newTitle
                , ruleMuts = ruleMuts
                }
            )


applyGrammarMut : GrammarMut -> Grammar
applyGrammarMut { oldGrammar, newTitle, ruleMuts } =
    { title = newTitle
    , rules =
        List.Extra.zip oldGrammar.rules ruleMuts
            |> List.map
                (\( ( nt, _ ), ruleMut ) -> ( nt, applyRuleMut ruleMut ))
    }
