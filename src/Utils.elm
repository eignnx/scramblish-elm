module Utils exposing (..)

import List.Extra
import Random


expect : (() -> String) -> Maybe a -> a
expect msg maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Debug.todo ("Expectation failure: `" ++ msg () ++ "`")


shuffled : List a -> Random.Generator (List a)
shuffled list =
    case list of
        [] ->
            Random.constant []

        _ ->
            let
                len =
                    List.length list

                chooseIndex : Random.Generator Int
                chooseIndex =
                    Random.uniform 0 (List.range 1 (len - 1))

                indicies : List Int
                indicies =
                    List.range 0 (len - 1)

                zipped : List ( Int, a )
                zipped =
                    List.Extra.zip indicies list

                removeChosen : Random.Generator ( a, List a )
                removeChosen =
                    chooseIndex
                        |> Random.map
                            (\chosenIndex ->
                                List.Extra.select list
                                    |> List.Extra.getAt chosenIndex
                                    |> expect
                                        (\() ->
                                            "in shuffled: index was chosen within bounds of remaining list: "
                                                ++ String.fromInt chosenIndex
                                                ++ ", list = "
                                                ++ Debug.toString list
                                        )
                            )
            in
            removeChosen
                |> Random.andThen
                    (\( chosen, remaining ) ->
                        Random.map2 (::)
                            (Random.constant chosen)
                            (shuffled remaining)
                    )


randomFlattenList : (a -> Random.Generator b) -> List a -> Random.Generator (List b)
randomFlattenList fn list =
    case list of
        [] ->
            Random.constant []

        x :: xs ->
            Random.map2 (::) (fn x) (randomFlattenList fn xs)
