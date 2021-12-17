module Directions exposing (..)

import Maybe exposing (andThen)



-- See https://thoughtbot.com/blog/maybe-mechanics


directions : String
directions =
    """forward 5
down 5
forward 8
up 3
down 8
forward 2"""


type Order
    = Up
    | Down
    | Forward
    | Back


parseOrder : String -> Maybe Order
parseOrder order =
    case order of
        "up" ->
            Just Up

        "down" ->
            Just Down

        "forward" ->
            Just Forward

        "back" ->
            Just Back

        _ ->
            Nothing


splitInstructions : String -> Maybe ( String, Int )
splitInstructions string =
    let
        words =
            String.words string
    in
    case words of
        [ a, b ] ->
            Maybe.map (\amount -> ( a, amount )) (String.toInt b)

        _ ->
            Nothing


parseDirections : String -> List ( String, Int )
parseDirections string =
    String.lines string
        |> List.filterMap splitInstructions


test : List ( String, Int )
test =
    parseDirections directions
