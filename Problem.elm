module Problem exposing (Problem, create, update)

import Attempt exposing (Attempt(..))
import Key exposing (AttemptKey(..))
import Letter exposing (Letter)
import Cons exposing (Cons)


type alias Problem =
    { hint : String
    , answer : Cons Letter
    , attempt : Maybe Attempt
    }


create : String -> Cons Letter -> Problem
create hint answer =
    { hint = hint
    , answer = answer
    , attempt = Nothing
    }


update : AttemptKey -> Problem -> Problem
update key problem =
    case problem.attempt of
        Just attempt ->
            { problem | attempt = Attempt.update key problem.answer attempt }

        Nothing ->
            case key of
                LetterKey letter ->
                    { problem | attempt = Just (Attempt.create letter problem.answer) }

                Backspace ->
                    problem
