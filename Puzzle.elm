module Puzzle exposing (..)

import Problem exposing (Problem)
import Key exposing (Key(..))
import Letter exposing (Letter)
import Cons exposing (Cons, cons)
import Attempt


type Puzzle
    = InProgress (Cons Problem)
    | Finished


create : Cons ( String, Cons Letter ) -> Puzzle
create problems =
    InProgress
        (Cons.map
            (\( hint, answer ) -> Problem.create hint answer)
            problems
        )


cycleNext : Cons Problem -> Cons Problem
cycleNext problems =
    case Cons.tail problems of
        [ head ] ->
            cons head [ Cons.head problems ]

        head :: rest ->
            cons head (rest ++ [ Cons.head problems ])

        [] ->
            problems


cyclePrevious : Cons Problem -> Cons Problem
cyclePrevious problems =
    case Cons.tail problems of
        [ head ] ->
            cons head [ Cons.head problems ]

        head :: rest ->
            let
                lastProblem =
                    Cons.head (Cons.reverse problems)

                remainingProblems =
                    List.take (List.length rest - 1) rest
            in
                cons lastProblem ([ Cons.head problems ] ++ remainingProblems)

        [] ->
            problems


update : Key -> Puzzle -> Puzzle
update key puzzle =
    case puzzle of
        InProgress problems ->
            case key of
                Space ->
                    InProgress (cycleNext problems)

                Down ->
                    InProgress (cycleNext problems)

                Up ->
                    InProgress (cyclePrevious problems)

                ProblemKey key ->
                    let
                        problem =
                            Cons.head problems

                        problem' =
                            Problem.update key problem

                        problems' =
                            cons problem' (Cons.tail problems)
                    in
                        case
                            Cons.all
                                (\{ attempt } ->
                                    case attempt of
                                        Just attempt' ->
                                            Attempt.complete attempt'

                                        Nothing ->
                                            False
                                )
                                problems'
                        of
                            True ->
                                Finished

                            False ->
                                InProgress problems'

                Unrecognised ->
                    puzzle

        Finished ->
            Finished
