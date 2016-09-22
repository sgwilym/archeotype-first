module Problem exposing (Problem, Result(..), create, toResult)

import String


type alias Guess =
    Char


type alias Letters =
    List Char


type Result
    = Wrong
    | Correct Letters
    | Solved


type alias Problem =
    { hint : String, solution : Letters }


create : String -> String -> Problem
create hint solution =
    { hint = hint
    , solution = String.toList solution
    }


toResult : Problem -> Guess -> Result
toResult puzzle guess =
    case List.head puzzle.solution of
        Just next ->
            if next == guess then
                case List.tail puzzle.solution of
                    Just tail ->
                        Correct tail

                    Nothing ->
                        Solved
            else
                Wrong

        Nothing ->
            Solved
