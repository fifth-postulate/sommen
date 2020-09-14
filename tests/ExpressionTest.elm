module ExpressionTest exposing (suite)

import Expect
import Expression exposing (..)
import Fuzz
import Json.Decode as Decode
import Shrink exposing (noShrink)
import Test exposing (Test, describe, fuzz)
import Json.Decode exposing (decodeValue)


suite : Test
suite =
    describe "Expression module"
        [ describe "Expression"
            [ let
                vs =
                    Between 10 100
                        |> value

                ops =
                    operator Addition [ Multiplication, Subtraction ]

                fuzzer =
                    Fuzz.custom (expression vs ops vs) noShrink
              in
              fuzz fuzzer "encodeExpression and decodeExpression are inverses" <|
                \expr ->
                    let
                        actual =
                            expr
                                |> encodeExpression
                                |> Decode.decodeValue decodeExpression
                    in
                    Expect.equal actual (Ok expr)
            ]
        , describe "Value" [
            fuzz Fuzz.int "encodeValue and decodeValue are inverses" <|
                \n ->
                    let
                        actual =
                            n
                            |> encodeValue
                            |> Decode.decodeValue Expression.decodeValue
                    in
                    Expect.equal actual (Ok n)
        ]
        ]
