module AdminHistoryTest exposing (..)

import Admin.History as History
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, fuzz2, test)


suite : Test
suite =
    describe "Admin module"
        [ describe "History module"
            [ fuzz Fuzz.int "empty history always return Nothing" <|
                \key ->
                    let
                        history =
                            History.empty
                    in
                    Expect.equal Nothing <| History.latest key history
            , test "History.latest returns an entry with corresponding key" <|
                \_ ->
                    let
                        key =
                            1

                        value =
                            1

                        history =
                            History.empty
                                |> History.push key value
                    in
                    Expect.equal (Just value) <| History.latest key history
            , fuzz2 Fuzz.int Fuzz.int "History.latest returns last pushed entry" <|
                \key value ->
                    let
                        history =
                            History.empty
                                |> History.push key 1
                                |> History.push key 2
                                |> History.push key value
                    in
                    Expect.equal (Just value) <| History.latest key history
             , fuzz2 Fuzz.int Fuzz.int "Histories for seperate keys are independ" <|
                \key1 key2 ->
                    let
                        history =
                            History.empty
                                |> History.push key1 1
                                |> History.push key2 2
                    in
                    Expect.equal (Just 1) <| History.latest key1 history
            ]
        ]
