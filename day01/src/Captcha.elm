module Captcha exposing (numberOfRepeats, splitPuzzle, total, wrapAroundValue, solve)

import List.Extra exposing (..)


-- import Dict.Extra exposing (..)


numberOfRepeats : List number -> List ( number, Int )
numberOfRepeats list =
    List.Extra.group list
        |> List.map (\group -> ( (List.head group |> Maybe.withDefault 0), (List.length group) - 1 ))


splitPuzzle : String -> List Int
splitPuzzle puzzle =
    String.split "" puzzle
        |> List.map (\s -> Result.withDefault -1 (String.toInt s))


total : List ( Int, Int ) -> Int
total list =
    List.filter (\pair -> (Tuple.second pair) > 0) list
        |> List.map (\pair -> (Tuple.first pair) * (Tuple.second pair))
        |> List.sum


wrapAroundValue : List ( Int, Int ) -> Int
wrapAroundValue list =
    let
        firstKey =
            Maybe.withDefault ( 0, 0 ) (List.head list) |> Tuple.first

        lastKey =
            Maybe.withDefault ( 0, 0 ) (List.head (List.reverse list)) |> Tuple.first
    in
        if (firstKey == lastKey) then
            firstKey
        else
            0


solve : String -> Int
solve puzzle =
    let
        repeatGroupings =
            splitPuzzle puzzle
                |> numberOfRepeats

        solution =
            (repeatGroupings |> total) + (repeatGroupings |> wrapAroundValue)
    in
        solution
