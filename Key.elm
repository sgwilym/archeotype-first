module Key exposing (Key(..), AttemptKey(..), Letter(..), fromKeyCode)

import Keyboard exposing (KeyCode)


type Key
    = ProblemKey AttemptKey
    | Space
    | Up
    | Down
    | Unrecognised


type AttemptKey
    = LetterKey Letter
    | Backspace


type Letter
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M
    | N
    | O
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z


fromKeyCode : KeyCode -> Key
fromKeyCode code =
    case code of
        65 ->
            ProblemKey (LetterKey A)

        66 ->
            ProblemKey (LetterKey B)

        67 ->
            ProblemKey (LetterKey C)

        68 ->
            ProblemKey (LetterKey D)

        69 ->
            ProblemKey (LetterKey E)

        70 ->
            ProblemKey (LetterKey F)

        71 ->
            ProblemKey (LetterKey G)

        72 ->
            ProblemKey (LetterKey H)

        73 ->
            ProblemKey (LetterKey I)

        74 ->
            ProblemKey (LetterKey J)

        75 ->
            ProblemKey (LetterKey K)

        76 ->
            ProblemKey (LetterKey L)

        77 ->
            ProblemKey (LetterKey M)

        78 ->
            ProblemKey (LetterKey N)

        79 ->
            ProblemKey (LetterKey O)

        80 ->
            ProblemKey (LetterKey P)

        81 ->
            ProblemKey (LetterKey Q)

        82 ->
            ProblemKey (LetterKey R)

        83 ->
            ProblemKey (LetterKey S)

        84 ->
            ProblemKey (LetterKey T)

        85 ->
            ProblemKey (LetterKey U)

        86 ->
            ProblemKey (LetterKey V)

        87 ->
            ProblemKey (LetterKey W)

        88 ->
            ProblemKey (LetterKey X)

        89 ->
            ProblemKey (LetterKey Y)

        90 ->
            ProblemKey (LetterKey Z)

        8 ->
            ProblemKey Backspace

        32 ->
            Space

        38 ->
            Up

        40 ->
            Down

        _ ->
            Unrecognised
