module Directions exposing (..)


directions : String
directions =
    """forward 5
down 5
forward 8
up 3
down 8
forward 2"""


splitInstructions : String -> ( String, String )
splitInstructions string =
    String.words string


parseDirections : String -> List String
parseDirections string =
    String.lines string
        |> List.map (\instruction -> String.words instruction)


test : List String
test =
    parseDirections directions
