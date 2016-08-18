module Utils exposing (..)

import String
import Char
import String.Extra exposing (replace)
import List.Extra exposing (dropWhile)


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


isSpace : Char -> Bool
isSpace x =
    if Char.toCode x == 32 then
        True
    else
        False



-- Cr (13) is the char produced by the enter key.


lfToCr : Char -> Char
lfToCr x =
    if Char.toCode x == 10 then
        Char.fromCode 13
    else
        x



-- String.words doesn't keep the line brakes. This implementation does.
-- Another difference is that it'll return a (List (List Char)) rahter than a (List String)


words : String -> List (List Char)
words xs =
    let
        step =
            \c acc ->
                case acc of
                    [] ->
                        []

                    x :: xs ->
                        if isSpace c then
                            ([] :: acc)
                        else
                            (c :: x) :: xs
    in
        List.foldr step [ [] ] (String.toList xs)


wordsToChars : List (List Char) -> List (Char)
wordsToChars =
    List.intersperse [ ' ' ]
        >> List.concat


dropHeaders : String -> String
dropHeaders =
    String.lines
        >> dropWhile (not << String.contains "***")
        >> List.drop 1
        >> dropWhile (\a -> String.contains "Produced" a || String.length a < 2)
        >> List.intersperse "\n"
        >> String.concat


removeExtraNewLines : String -> String
removeExtraNewLines =
    replace "\n\n" "$^"
        >> replace "\n" " "
        >> replace "$^" "\x0D"
