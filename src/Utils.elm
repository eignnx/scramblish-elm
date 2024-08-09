module Utils exposing (..)

import List.Extra
import Random
import Task
import Time


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


{-| Build a list of values generated randomly based on an existing list.
-}
randomFlattenList : (a -> Random.Generator b) -> List a -> Random.Generator (List b)
randomFlattenList fn list =
    case list of
        [] ->
            Random.constant []

        x :: xs ->
            Random.map2 (::) (fn x) (randomFlattenList fn xs)


sequenceRandom : List (a -> Random.Generator a) -> a -> Random.Generator a
sequenceRandom generators initial =
    case generators of
        [] ->
            Random.constant initial

        f :: fs ->
            f initial |> Random.andThen (\a -> sequenceRandom fs a)


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


indexed : List a -> List ( Int, a )
indexed xs =
    List.Extra.zip
        (List.range 0 (List.length xs - 1))
        xs


randomChoice : a -> List a -> Random.Generator a
randomChoice default choices =
    case choices of
        [] ->
            Random.constant default

        x :: xs ->
            Random.uniform x xs


stringHash : String -> Int
stringHash s =
    let
        update : Int -> Int -> Int
        update digest incoming =
            -- probably terrible hash function
            modBy (digest + incoming * 123451 + 9876543) (1024 * 1024)
    in
    s
        |> String.toList
        |> List.map Char.toCode
        |> List.foldl update 0



-- CMD


doCmd : msg -> Cmd msg
doCmd msg =
    Time.now |> Task.perform (\_ -> msg)
