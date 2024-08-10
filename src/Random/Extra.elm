module Random.Extra exposing (..)

import List.Extra
import Maybe.Extra
import Random


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

                removeChosen : Random.Generator ( a, List a )
                removeChosen =
                    chooseIndex
                        |> Random.map
                            (\chosenIndex ->
                                List.Extra.select list
                                    |> List.Extra.getAt chosenIndex
                                    |> Maybe.Extra.expect
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


{-| Build a list of values generated randomly based on an existing list.
-}
flattenList : (a -> Random.Generator b) -> List a -> Random.Generator (List b)
flattenList fn list =
    case list of
        [] ->
            Random.constant []

        x :: xs ->
            Random.map2 (::) (fn x) (flattenList fn xs)


sequence : List (a -> Random.Generator a) -> a -> Random.Generator a
sequence generators initial =
    case generators of
        [] ->
            Random.constant initial

        f :: fs ->
            f initial |> Random.andThen (\a -> sequence fs a)


chance : Float -> Random.Generator Bool
chance percent =
    Random.float 0 1 |> Random.map (\r -> r < percent)


choose : Float -> (() -> Random.Generator a) -> (() -> Random.Generator a) -> Random.Generator a
choose percent consequent alternative =
    chance percent
        |> Random.andThen
            (\b ->
                if b then
                    consequent ()

                else
                    alternative ()
            )


choice : a -> List a -> Random.Generator a
choice default choices =
    case choices of
        [] ->
            Random.constant default

        x :: xs ->
            Random.uniform x xs
