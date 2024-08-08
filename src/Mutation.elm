module Mutation exposing (..)

import Dict
import Grammar exposing (Form(..), Grammar, Nt, SententialForm, SyntaxTree(..), Tm(..), lookupNt)
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


applyRuleMut : GrammarMut -> RuleMut -> SententialForm
applyRuleMut gm { clausePermutation, oldSententialForm } =
    List.Extra.zip clausePermutation oldSententialForm
        |> List.sortBy (\( newIndex, _ ) -> newIndex)
        |> List.map (\( _, value ) -> value)
        |> List.map
            (\form ->
                case form of
                    TmForm (Tm word) ->
                        Dict.get word gm.wordMapping |> Maybe.withDefault "[untranslatable]" |> Tm |> TmForm

                    other ->
                        other
            )


type alias GrammarMut =
    { oldGrammar : Grammar
    , newTitle : String
    , ruleMuts : List RuleMut
    , wordMapping : WordMapping
    }


grammarMutGenerator : String -> Grammar -> Random.Generator GrammarMut
grammarMutGenerator newTitle grammar =
    let
        wordMapping : Random.Generator WordMapping
        wordMapping =
            wordMappingGenerator grammar

        ruleMutations : Random.Generator (List RuleMut)
        ruleMutations =
            Utils.randomFlattenList ruleMutGenerator grammar.rules

        buildGrammarMutation ruleMuts wmap =
            { oldGrammar = grammar
            , newTitle = newTitle
            , ruleMuts = ruleMuts
            , wordMapping = wmap
            }
    in
    Random.map2
        buildGrammarMutation
        ruleMutations
        wordMapping


applyGrammarMut : GrammarMut -> Grammar
applyGrammarMut gm =
    { title = gm.newTitle
    , rules =
        List.Extra.zip gm.oldGrammar.rules gm.ruleMuts
            |> List.map
                (\( ( nt, _ ), ruleMut ) -> ( nt, applyRuleMut gm ruleMut ))
    }


mutateSyntaxTree : GrammarMut -> SyntaxTree -> SyntaxTree
mutateSyntaxTree grammarMut oldTree =
    case oldTree of
        Leaf (Tm oldTm) ->
            Leaf (Tm (Maybe.withDefault "[untranslatable]" (Dict.get oldTm grammarMut.wordMapping)))

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



-- WORD MUTATION


type alias WordMapping =
    Dict.Dict String String


wordMappingGenerator : Grammar -> Random.Generator WordMapping
wordMappingGenerator { rules } =
    let
        allSrcTerminals : List String
        allSrcTerminals =
            rules
                |> List.concatMap
                    (\( _, sf ) ->
                        sf
                            |> List.filterMap
                                (\form ->
                                    case form of
                                        NtForm _ ->
                                            Nothing

                                        TmForm (Tm word) ->
                                            Just word
                                )
                    )
    in
    allSrcTerminals
        |> Utils.randomFlattenList
            (\word ->
                Random.pair
                    (Random.constant word)
                    (randomWordGenerator word)
            )
        |> Random.map Dict.fromList


randomWordGenerator : String -> Random.Generator String
randomWordGenerator eng =
    Random.constant (String.reverse eng)
