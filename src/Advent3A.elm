module Advent3A exposing (..)

import Power exposing (powerSample)


parseLines : String -> List String
parseLines string =
    String.lines string


charToBool : Char -> Maybe Bool
charToBool char =
    case char of
        0 ->
            Just False

        1 ->
            Just True

        _ ->
            Nothing


convertLineToBinaries : String -> Maybe List Bool
convertLineToBinaries string =
    if String.length string /= 5 then
        Nothing

    else
        string
            |> String.toList
            |> List.filterMap charToBool


test : List String
test =
    parseLines powerSample
