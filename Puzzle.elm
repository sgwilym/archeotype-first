module Puzzle exposing (Puzzle, create, toProgress)

import Problem exposing (Problem)


type alias Puzzle =
    List Problem


type Progress
    = Continue Problem
    | Finish


create : List ( String, String ) -> Puzzle
create problems =
    List.map
        (\( hint, solution ) -> Problem.create hint solution)
        problems


toProgress : Puzzle -> Problem -> Problem.Result -> Progress
toProgress stage puzzle result =
    case result of
        Problem.Wrong ->
            Continue puzzle

        Problem.Correct letters ->
            Continue { puzzle | solution = letters }

        Problem.Solved ->
            case List.tail stage of
                Just tail ->
                    case List.head tail of
                        Just puzzle ->
                            Continue puzzle

                        Nothing ->
                            Finish

                Nothing ->
                    Finish
