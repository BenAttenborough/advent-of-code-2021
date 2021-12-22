module Advent1a exposing (..)

import Depths exposing (depthsFinal)


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
                    if x > y then
                        count

                    else
                        count + 1
            in
            countOfIncreases (y :: rest) intermediateCount


test : Int
test =
    countOfIncreases depthsFinal 0
