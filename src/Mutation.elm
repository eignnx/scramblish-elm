module Mutation exposing (..)

import Grammar exposing (Grammar, Nt, SententialForm, SyntaxTree(..), Tm(..), lookupNt)
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


mutateSyntaxTree : GrammarMut -> SyntaxTree -> SyntaxTree
mutateSyntaxTree grammarMut oldTree =
    case oldTree of
        Leaf (Tm oldTm) ->
            Leaf (Tm oldTm)

        Node { nt, branchIndex, children } ->
            let
                ruleMuts : List RuleMut
                ruleMuts =
                    grammarMut.ruleMuts
                        |> List.map (\m -> ( m.lhs, m ))
                        |> (\ms -> lookupNt ms nt)

                ruleMut : RuleMut
                ruleMut =
                    ruleMuts
                        |> List.Extra.getAt branchIndex
                        |> Utils.expect
                            (\() ->
                                "Branch index `"
                                    ++ String.fromInt branchIndex
                                    ++ "` is in bounds of rule mutation list: "
                                    ++ Debug.toString ruleMuts
                            )

                newChildrenUnpermuted : List SyntaxTree
                newChildrenUnpermuted =
                    children
                        |> List.map (mutateSyntaxTree grammarMut)

                getPermutationIndex : Int -> SyntaxTree
                getPermutationIndex index =
                    List.Extra.getAt index newChildrenUnpermuted
                        |> Utils.expect
                            (\() ->
                                "Permutation indices `"
                                    ++ Debug.toString ruleMut.clausePermutation
                                    ++ "` are all valid indicies of list `"
                                    ++ Debug.toString newChildrenUnpermuted
                                    ++ "`"
                            )

                newChildren : List SyntaxTree
                newChildren =
                    ruleMut.clausePermutation
                        |> List.map getPermutationIndex
            in
            Node { nt = nt, branchIndex = branchIndex, children = newChildren }
