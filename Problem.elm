module Problem exposing (Problem, Letters, Result(..), create, toResult)

import Cons exposing (Cons, cons)


type alias Guess =
    Char


type alias Letters =
    Cons Char


type Result
    = Wrong
    | Correct Letters
    | Solved


type alias Problem =
    { hint : String, solution : Letters }


create : String -> Cons Char -> Problem
create hint solution =
    { hint = hint
    , solution = solution
    }


toResult : Problem -> Guess -> Result
toResult { solution } guess =
    if guess == Cons.head solution then
        case Cons.tail solution of
            head :: tail ->
                Correct (cons head tail)

            [] ->
                Solved
    else
        Wrong
