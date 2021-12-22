module Advent1b exposing (..)

-- import Advent1 exposing (countOfIncreases)

import Depths exposing (depths2, depthsExample, depthsExampleMod, depthsFinal)


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
    sumWindows depthsFinal []
        |> List.reverse
        |> (\list -> countOfIncreases list 0)


testA : List Int
testA =
    sumWindows depths2 []
        |> List.reverse


countOfIncreases : List Int -> Int -> Int
countOfIncreases list count =
    case list of
        [] ->
            count

        [ _ ] ->
            count

        x :: y :: rest ->
            let
                intermediateCount =
                    if y > x then
                        count + 1

                    else
                        count
            in
            countOfIncreases (y :: rest) intermediateCount
