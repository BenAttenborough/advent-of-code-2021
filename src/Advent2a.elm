module Advent2a exposing (..)

-- See https://thoughtbot.com/blog/maybe-mechanics

import Directions exposing (directions)


type Order
    = Up Int
    | Down Int
    | Forward Int
    | Back Int


parseOrder : String -> Int -> Maybe Order
parseOrder order amount =
    case order of
        "up" ->
            Just (Up amount)

        "down" ->
            Just (Down amount)

        "forward" ->
            Just (Forward amount)

        "back" ->
            Just (Back amount)

        _ ->
            Nothing


convertLinesToOrders : String -> Maybe Order
convertLinesToOrders string =
    case String.words string of
        [ a, b ] ->
            let
                maybeAmount =
                    String.toInt b
            in
            maybeAmount
                |> Maybe.andThen (\amount -> parseOrder a amount)

        _ ->
            Nothing


parseDirections : String -> List Order
parseDirections string =
    String.lines string
        |> List.filterMap convertLinesToOrders


executeOrders : ( Int, Int ) -> List Order -> ( Int, Int )
executeOrders position orders =
    let
        x =
            Tuple.first position

        y =
            Tuple.second position
    in
    case orders of
        [] ->
            position

        first :: rest ->
            case first of
                Up val ->
                    executeOrders ( x, y - val ) rest

                Down val ->
                    executeOrders ( x, y + val ) rest

                Forward val ->
                    executeOrders ( x + val, y ) rest

                Back val ->
                    executeOrders ( x - val, y ) rest


test : Int
test =
    parseDirections directions
        |> executeOrders ( 0, 0 )
        |> (\val -> Tuple.first val * Tuple.second val)
