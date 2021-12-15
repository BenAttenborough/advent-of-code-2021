module Advent2 exposing (..)

import Advent1 exposing (countOfIncreases)
import Depths exposing (depthsExample)


sumWindows : List Int -> List Int -> List Int
sumWindows input output =
    case input of
        [] ->
            output

        [ _ ] ->
            output

        [ _, _ ] ->
            output

        a :: b :: c :: rest ->
            sumWindows (b :: c :: rest) (a + b + c :: output)


test : Int
test =
    sumWindows depthsExample []
        |> List.reverse
        |> (\list -> countOfIncreases list 0)
