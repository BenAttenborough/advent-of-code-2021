module Advent1 exposing (..)

import Depths exposing (depths)


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
    countOfIncreases depths 0
