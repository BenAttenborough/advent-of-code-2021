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


splitInstructions : String -> Maybe ( String, String )
splitInstructions string =
    let
        words =
            String.words string
    in
    case words of
        [ a, b ] ->
            Just ( a, b )

        _ ->
            Nothing


parseFirstValue : ( String, String ) -> Maybe ( Order, String )
parseFirstValue instructions =
    let
        amount =
            Tuple.second instructions

        maybeOrder =
            Tuple.first instructions
                |> parseOrder
    in
    Maybe.map (\value -> ( value, amount )) maybeOrder


parseSecondValue : ( Order, String ) -> Maybe ( Order, Int )
parseSecondValue instructions =
    let
        first =
            Tuple.first instructions

        second =
            Tuple.second instructions
                |> String.toInt
    in
    Maybe.map (\value -> ( first, value )) second


parseDirections : String -> List ( Order, Int )
parseDirections string =
    String.lines string
        |> List.filterMap splitInstructions
        |> List.filterMap parseFirstValue
        |> List.filterMap parseSecondValue


test : List ( Order, Int )
test =
    parseDirections directions
