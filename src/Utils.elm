module Utils exposing (..)

import List.Extra
import Task
import Time



-- LIST


indexed : List a -> List ( Int, a )
indexed xs =
    List.Extra.zip
        (List.range 0 (List.length xs - 1))
        xs


resultTranspose : Result e (List t) -> List (Result e t)
resultTranspose res =
    case res of
        Ok ts ->
            List.map Ok ts

        Err e ->
            [ Err e ]


tryCollect : List (Result e t) -> Result e (List t)
tryCollect results =
    case results of
        [] ->
            Ok []

        (Err e) :: _ ->
            Err e

        (Ok t) :: rs ->
            tryCollect rs |> Result.map (\rs2 -> t :: rs2)



-- STRING


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
