module Attempt exposing (Attempt(..), create, update, last, complete, toLetters, length)

import Key exposing (AttemptKey(..))
import Letter exposing (Letter)
import Cons exposing (Cons, cons)


type Attempt
    = Success Letter Attempt
    | Failure Letter
    | Complete Letter
    | Incomplete Letter


create : Letter -> Cons Letter -> Attempt
create letter answer =
    fromLetters (cons letter []) (Cons.toList answer)


update : AttemptKey -> Cons Letter -> Attempt -> Maybe Attempt
update key answer attempt =
    case key of
        LetterKey letter ->
            let
                letters =
                    Cons.append (toLetters attempt) (cons letter [])

                attempt' =
                    fromLetters letters (Cons.toList answer)
            in
                Just attempt'

        Backspace ->
            case Cons.tail (toLetters attempt) of
                [ head ] ->
                    Just
                        (fromLetters (cons (Cons.head (toLetters attempt)) []) (Cons.toList answer))

                head :: tail ->
                    let
                        length =
                            List.length tail

                        remainingTail =
                            List.take (length - 1) tail
                    in
                        Just
                            ((fromLetters (cons (Cons.head (toLetters attempt)) ([ head ] ++ remainingTail)))
                                (Cons.toList answer)
                            )

                [] ->
                    Nothing


toLetters : Attempt -> Cons Letter
toLetters attempt =
    case attempt of
        Success letter attempt' ->
            cons letter (Cons.toList (toLetters attempt'))

        Failure letter ->
            cons letter []

        Complete letter ->
            cons letter []

        Incomplete letter ->
            cons letter []


fromLetters : Cons Letter -> List Letter -> Attempt
fromLetters letters answer =
    case answer of
        [ head ] ->
            case head == Cons.head letters of
                True ->
                    Complete (Cons.head letters)

                False ->
                    Failure (Cons.head letters)

        head :: tail ->
            case head == Cons.head letters of
                True ->
                    case Cons.tail letters of
                        guessHead :: guessTail ->
                            Success (Cons.head letters) (fromLetters (cons guessHead guessTail) tail)

                        [] ->
                            Incomplete head

                False ->
                    Failure (Cons.head letters)

        [] ->
            Complete (Cons.head letters)


length : Attempt -> Int
length attempt =
    case attempt of
        Success _ attempt ->
            1 + length attempt

        _ ->
            1


last : Attempt -> Attempt
last attempt =
    case attempt of
        Success _ attempt' ->
            last attempt'

        _ ->
            attempt


complete : Attempt -> Bool
complete attempt =
    case last attempt of
        Complete _ ->
            True

        _ ->
            False
