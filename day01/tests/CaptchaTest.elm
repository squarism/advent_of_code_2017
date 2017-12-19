module CaptchaTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Captcha exposing (..)


suite : Test
suite =
    describe "Captcha"
        [ describe "Number of Repeats without wrapping around"
            [ test "First case of 1122" <|
                \_ ->
                    let
                        puzzle =
                            [ 1, 1, 2, 2 ]

                        solution =
                            Captcha.numberOfRepeats puzzle
                    in
                        Expect.equal solution [ ( 1, 1 ), ( 2, 1 ) ]
            , test "Second case 1111" <|
                \_ ->
                    let
                        puzzle =
                            [ 1, 1, 1, 1 ]

                        solution =
                            Captcha.numberOfRepeats puzzle
                    in
                        Expect.equal solution [ ( 1, 3 ) ]
            , test "Third case 1234" <|
                \_ ->
                    let
                        puzzle =
                            [ 1, 2, 3, 4 ]

                        solution =
                            Captcha.numberOfRepeats puzzle
                    in
                        Expect.equal solution [ ( 1, 0 ), ( 2, 0 ), ( 3, 0 ), ( 4, 0 ) ]
            , test "Fourth case 91212129" <|
                \_ ->
                    let
                        puzzle =
                            [ 9, 1, 2, 1, 2, 1, 2, 9 ]

                        solution =
                            Captcha.numberOfRepeats puzzle
                    in
                        Expect.equal solution [ ( 9, 0 ), ( 1, 0 ), ( 2, 0 ), ( 1, 0 ), ( 2, 0 ), ( 1, 0 ), ( 2, 0 ), ( 9, 0 ) ]
            ]
        , describe "The puzzle is given to us as a long string of numbers"
            [ test "Splits a string into a list of ints" <|
                \_ ->
                    let
                        solution =
                            Captcha.splitPuzzle "1234"
                    in
                        Expect.equal solution [ 1, 2, 3, 4 ]
            , test "Handles bad data" <|
                \_ ->
                    let
                        solution =
                            Captcha.splitPuzzle "12a4"
                    in
                        Expect.equal solution [ 1, 2, -1, 4 ]
            ]
        , describe "Totaling up the repeated values"
            [ test "Repeats greater than 0 are multiplied by its key and then summed" <|
                \_ ->
                    let
                        groupings =
                            [ ( 1, 1 ), ( 2, 0 ), ( 3, 2 ), ( 4, 0 ) ]

                        total =
                            Captcha.total groupings
                    in
                        Expect.equal total 7
            , describe "Dealing with the wrap around rule"
                [ test "Groupings wrap from the last to the first" <|
                    \_ ->
                        let
                            groupings =
                                [ ( 9, 0 ), ( 2, 0 ), ( 3, 2 ), ( 9, 4 ) ]

                            wrapAroundAddition =
                                Captcha.wrapAroundValue groupings
                        in
                            Expect.equal wrapAroundAddition 9
                , test "Groupings don't wrap around" <|
                    \_ ->
                        let
                            groupings =
                                [ ( 8, 0 ), ( 2, 0 ), ( 3, 2 ), ( 9, 4 ) ]

                            wrapAroundAddition =
                                Captcha.wrapAroundValue groupings
                        in
                            Expect.equal wrapAroundAddition 0
                ]
            ]
        ]
