module Utils exposing (..)


foldl2 : (a -> b -> c -> c) -> c -> List a -> List b -> c
foldl2 f acc list1 list2 =
    case ( list1, list2 ) of
        ( [], _ ) ->
            acc

        ( _, [] ) ->
            acc

        ( x :: xs, y :: ys ) ->
            foldl2 f (f x y acc) xs ys


atLeast min num =
    if num > min then
        num
    else
        min
