module Puzzle exposing (..)

import Problem exposing (Problem)
import Key exposing (Key(..))
import Letter exposing (Letter)
import Cons exposing (Cons, cons)
import Attempt
import Json.Decode as Json
import String
import Result.Extra


type Puzzle
    = InProgress (Cons Problem) (List Problem)
    | Finished


create : Cons ( String, Cons Letter ) -> Puzzle
create problems =
    InProgress
        (Cons.map
            (\( hint, answer ) -> Problem.create hint answer)
            problems
        )
        []


remainingLetters : Puzzle -> Int
remainingLetters puzzle =
    case puzzle of
        InProgress remaining solved ->
            let
                remainingCount =
                    Cons.map (Problem.remaining) remaining
                        |> Cons.foldl1 (+)

                solvedCount =
                    List.map (Problem.remaining) solved
                        |> List.foldl (+) 0
            in
                remainingCount - solvedCount

        Finished ->
            0


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
                    List.take (List.length (Cons.tail problems) - 1) (Cons.tail problems)
            in
                cons lastProblem ([ Cons.head problems ] ++ remainingProblems)

        [] ->
            problems


progress : Puzzle -> Puzzle
progress puzzle =
    case puzzle of
        InProgress problems solved ->
            case Cons.tail problems of
                [ head ] ->
                    case Problem.isComplete head of
                        True ->
                            InProgress (cons (Cons.head problems) []) (solved ++ [ head ])

                        False ->
                            puzzle

                head :: rest ->
                    InProgress
                        (cons (Cons.head problems) (List.filter Problem.isIncomplete ([ head ] ++ rest)))
                        (List.filter Problem.isComplete ([ head ] ++ rest ++ solved))

                [] ->
                    case Problem.isComplete (Cons.head problems) of
                        True ->
                            Finished

                        False ->
                            puzzle

        Finished ->
            puzzle


update : Key -> Puzzle -> Puzzle
update key puzzle =
    case puzzle of
        InProgress problems solved ->
            case key of
                Space ->
                    progress (InProgress (cycleNext problems) solved)

                Down ->
                    progress (InProgress (cycleNext problems) solved)

                Up ->
                    progress (InProgress (cyclePrevious problems) solved)

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
                                InProgress problems' solved

                Unrecognised ->
                    puzzle

        Finished ->
            Finished


fromJson : Json.Decoder (Cons ( String, Cons Letter ))
fromJson =
    let
        toProblemResult ( answer, hint ) =
            case Cons.fromList (List.map Letter.fromChar (String.toList answer)) of
                Just answer' ->
                    Result.Ok ( hint, answer' )

                Nothing ->
                    Result.Err "No letters found in a answer!"
    in
        Json.customDecoder
            (Json.keyValuePairs Json.string)
            (\jsonList ->
                case jsonList of
                    [ head ] ->
                        case toProblemResult head of
                            Result.Ok problem ->
                                Result.Ok (cons problem [])

                            Result.Err error ->
                                Result.Err error

                    head :: tail ->
                        let
                            results =
                                List.map toProblemResult ([ head ] ++ tail)
                        in
                            case results of
                                head :: tail ->
                                    case Result.Extra.combine results of
                                        Result.Ok goodResults ->
                                            case Cons.fromList goodResults of
                                                Just puzzle ->
                                                    Result.Ok puzzle

                                                Nothing ->
                                                    Result.Err "Couldn't make the good results into a puzzle…"

                                        Result.Err error ->
                                            Result.Err error

                                [] ->
                                    Result.Err "No problems found in Json!"

                    [] ->
                        Result.Err "No problems found in Json!"
            )
