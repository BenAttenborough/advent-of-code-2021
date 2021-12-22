module Advent2b exposing (..)

-- See https://thoughtbot.com/blog/maybe-mechanics

import Directions exposing (directions, directionsSample)


type Order
    = Up Int
    | Down Int
    | Forward Int


parseOrder : String -> Int -> Maybe Order
parseOrder order amount =
    case order of
        "up" ->
            Just (Up amount)

        "down" ->
            Just (Down amount)

        "forward" ->
            Just (Forward amount)

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


executeOrders : State -> List Order -> State
executeOrders state orders =
    case orders of
        [] ->
            state

        first :: rest ->
            case first of
                Up val ->
                    executeOrders { state | aim = state.aim - val } rest

                Down val ->
                    executeOrders { state | aim = state.aim + val } rest

                Forward val ->
                    executeOrders { state | x = state.x + val, y = state.y + (state.aim * val) } rest


type alias State =
    { x : Int
    , y : Int
    , aim : Int
    }


test : Int
test =
    let
        state =
            State 0 0 0
    in
    parseDirections directions
        |> executeOrders state
        |> (\result -> result.x * result.y)



-- |> (\val -> Tuple.first val * Tuple.second val)
