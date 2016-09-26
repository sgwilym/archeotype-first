module Letter exposing (Letter(..), transposeCase, toChar, fromChar)

import Keyboard exposing (KeyCode)
import Char


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


transposeCase : KeyCode -> KeyCode
transposeCase number =
    if number >= 97 && number <= 122 then
        number - 32
    else
        number


fromChar : Char -> Letter
fromChar char =
    case Char.toUpper char of
        'A' ->
            A

        'B' ->
            B

        'C' ->
            C

        'D' ->
            D

        'E' ->
            E

        'F' ->
            F

        'G' ->
            G

        'H' ->
            H

        'I' ->
            I

        'J' ->
            J

        'K' ->
            K

        'L' ->
            L

        'M' ->
            M

        'N' ->
            N

        'O' ->
            O

        'P' ->
            P

        'Q' ->
            Q

        'R' ->
            R

        'S' ->
            S

        'T' ->
            T

        'U' ->
            U

        'V' ->
            V

        'W' ->
            W

        'X' ->
            X

        'Y' ->
            Y

        'Z' ->
            Z

        _ ->
            Z


toChar : Letter -> Char
toChar letter =
    case letter of
        A ->
            'A'

        B ->
            'B'

        C ->
            'C'

        D ->
            'D'

        E ->
            'E'

        F ->
            'F'

        G ->
            'G'

        H ->
            'H'

        I ->
            'I'

        J ->
            'J'

        K ->
            'K'

        L ->
            'L'

        M ->
            'M'

        N ->
            'N'

        O ->
            'O'

        P ->
            'P'

        Q ->
            'Q'

        R ->
            'R'

        S ->
            'S'

        T ->
            'T'

        U ->
            'U'

        V ->
            'V'

        W ->
            'W'

        X ->
            'X'

        Y ->
            'Y'

        Z ->
            'Z'
