module RomanConvert exposing (..)

import Dict exposing (Dict, fromList)
import Set exposing (..)


-- Elm implementation of converting to and from Integer and a string representation of a Roman Numeral
-- (C)2018 Justin Heyes-Jones
-- These are the valid characters that make up Roman Numerals


validChars : Set Char
validChars =
    Set.fromList [ 'I', 'V', 'X', 'L', 'C', 'D', 'M' ]



-- True if c is a valid Roman Numeral character


validRomanChar : Char -> Bool
validRomanChar c =
    member c validChars



-- Numeric values of each numeral


vm : Dict Char Int
vm =
    Dict.fromList
        [ ( 'I', 1 )
        , ( 'V', 5 )
        , ( 'X', 10 )
        , ( 'L', 50 )
        , ( 'C', 100 )
        , ( 'D', 500 )
        , ( 'M', 1000 )
        ]



-- Convert a string of input roman numerals and return the integer value
-- or zero if no valid integer was found


romanNumeralsToDecimal : String -> Int
romanNumeralsToDecimal input =
    let
        nums =
            String.toList input

        strVals =
            List.map (\n -> Dict.get n vm |> Maybe.withDefault 0) nums

        paired =
            pairUp 0 strVals

        folded =
            List.foldl
                (\( a, b ) acc ->
                    if a >= b then
                        acc + a
                    else
                        acc + (b - a) - b
                )
                0
                paired
    in
    folded


folded s =
    String.foldl
        (\b acc ->
            acc + Maybe.withDefault 0 (Dict.get b vm)
        )
        0
        s



-- pairUp, below, uses this function as a recursive helper to do its work


pairUpHelper : a -> List a -> List ( a, a ) -> List ( a, a )
pairUpHelper default n acc =
    case n of
        a :: b :: rest ->
            pairUpHelper default (b :: rest) (( a, b ) :: acc)

        [ a ] ->
            ( a, default ) :: acc

        [] ->
            acc



-- This pairs up the values in two lists. If one list is longer the paired value is the default value passed in


pairUp : a -> List a -> List ( a, a )
pairUp default n =
    pairUpHelper default n [] |> List.reverse
