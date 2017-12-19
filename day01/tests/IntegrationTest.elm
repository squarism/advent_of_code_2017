module IntegrationTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Captcha exposing (..)


suite : Test
suite =
    describe "Integration Tests"
        [ test "Example puzzle 1122" <|
            \_ ->
                Expect.equal (Captcha.solve "1122") 3
        , test "Example puzzle 1111" <|
            \_ ->
                Expect.equal (Captcha.solve "1111") 4
        , test "Example puzzle 1234" <|
            \_ ->
                Expect.equal (Captcha.solve "1234") 0
        , test "Example puzzle 91212129" <|
            \_ ->
                Expect.equal (Captcha.solve "91212129") 9
        ]
