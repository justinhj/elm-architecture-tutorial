module RomanConvert exposing (..)

import Dict exposing (Dict, fromList)
import Set exposing (..)


-- These are the valid characters that make up Roman Numerals


validChars : Set Char
validChars =
    Set.fromList [ 'I', 'V', 'X', 'L', 'C', 'D', 'M' ]


validRomanChar : Char -> Bool
validRomanChar c =
    member c validChars


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



-- TODO API should probably return Maybe so this is total


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
                (\ab acc ->
                    case ab of
                        ( a, b ) ->
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


helper : a -> List a -> List ( a, a ) -> List ( a, a )
helper default n acc =
    case n of
        a :: b :: rest ->
            helper default (b :: rest) (( a, b ) :: acc)

        [ a ] ->
            ( a, default ) :: acc

        [] ->
            acc


pairUp : a -> List a -> List ( a, a )
pairUp default n =
    helper default n [] |> List.reverse
