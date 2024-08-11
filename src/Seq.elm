module Seq exposing (..)

{-| Implementation of the OCaml `Seq` module. Most documentation comments come
from: <https://caml.inria.fr/pub/docs/manual-ocaml/libref/Seq.html>

The type `Seq a` is a delayed list, i.e. a list where some evaluation is
needed to access the next element. This makes it possible to build infinite
sequences, to build sequences as we traverse them, and to transform them in
a lazy fashion rather than upfront.

-}


{-| The type of delayed lists containing elements of type `a`. Note that the
concrete list node `Node a` is delayed under a closure, not a lazy block,
which means it might be recomputed every time we access it.
-}
type alias Seq a =
    () -> Node a


{-| A fully-evaluated list node, either empty or containing an element and a delayed tail.
-}
type Node a
    = Nil
    | Cons a (Seq a)


{-| Helper function to get the next `Node` in the list.
-}
next : Seq a -> Node a
next seq =
    seq ()


{-| The empty sequence, containing no elements.
-}
empty : Seq a
empty =
    \() -> Nil


{-| The singleton sequence containing only the given element.
-}
singleton : a -> Seq a
singleton x =
    \() -> Cons x empty


{-| Converts a sequence into a list.
-}
toList : Seq a -> List a
toList seq =
    case next seq of
        Nil ->
            []

        Cons x xs ->
            x :: toList xs


{-| Converts a list into a sequence. This transformation is lazy, it only
applies when the result is traversed.
-}
fromList : List a -> Seq a
fromList list =
    case list of
        [] ->
            \() -> Nil

        x :: xs ->
            \() -> Cons x (fromList xs)


{-| `map f seq` returns a new sequence whose elements are the elements of `seq`,
transformed by `f`. This transformation is lazy, it only applies when the
result is traversed.

If `seq == fromList [1, 2, 3]`, then `map f seq == fromList [f 1, f 2, f 3]`.

-}
map : (a -> b) -> Seq a -> Seq b
map f seq =
    case next seq of
        Nil ->
            \() -> Nil

        Cons x xs ->
            \() -> Cons (f x) (map f xs)


{-| Remove from the sequence the elements that do not satisfy the given
predicate. This transformation is lazy, it only applies when the result is
traversed.
-}
filter : (a -> Bool) -> Seq a -> Seq a
filter f seq =
    case next seq of
        Nil ->
            \() -> Nil

        Cons x xs ->
            if f x then
                \() -> Cons x (filter f xs)

            else
                filter f xs


{-| Apply the function to every element; if `f x == Nothing` then `x` is dropped;
if `f x == Just y` then `y` is returned. This transformation is lazy, it only
applies when the result is traversed.
-}
filterMap : (a -> Maybe b) -> Seq a -> Seq b
filterMap f seq =
    case next seq of
        Nil ->
            \() -> Nil

        Cons x xs ->
            case f x of
                Nothing ->
                    filterMap f xs

                Just y ->
                    \() -> Cons y (filterMap f xs)


{-| Append all of the first sequence onto the end of the second sequence. This
transformation is lazy, it only applies when the result is traversed.
-}
append : Seq a -> Seq a -> Seq a
append seq1 seq2 =
    case next seq1 of
        Nil ->
            seq2

        Cons x xs ->
            \() -> Cons x (append xs seq2)


{-| Map each element to a subsequence, then return each element of this
sub-sequence in turn. This transformation is lazy, it only applies when the
result is traversed.
-}
flatMap : (a -> Seq b) -> Seq a -> Seq b
flatMap f seq =
    case next seq of
        Nil ->
            \() -> Nil

        Cons x xs ->
            append (f x) (flatMap f xs)


{-| Traverse the sequence from left to right, combining each element with the
accumulator using the given function. The traversal happens immediately and
will not terminate on infinite sequences.
-}
foldl : (acc -> ele -> acc) -> acc -> Seq ele -> acc
foldl f acc seq =
    case next seq of
        Nil ->
            acc

        Cons x xs ->
            foldl f (f acc x) xs
