module Maybe.Extra exposing (..)


expect : (() -> String) -> Maybe a -> a
expect msg maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Debug.todo ("Expectation failure: `" ++ msg () ++ "`")


fold : acc -> (acc -> x -> Maybe acc) -> List x -> Maybe acc
fold acc1 f xs =
    case xs of
        [] ->
            Just acc1

        y :: ys ->
            f acc1 y
                |> Maybe.andThen (\acc2 -> fold acc2 f ys)
