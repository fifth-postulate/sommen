module Sommen exposing (..)

import Browser
import Expression exposing (Operator(..), Range(..), operator, value)
import Quiz


main =
    Browser.element
        { init = init
        , view = Quiz.view
        , update = Quiz.update
        , subscriptions = subscriptions
        }


init : () -> ( Quiz.Model, Cmd Quiz.Message )
init _ =
    let
        vs =
            value <| Between 10 50

        ops =
            operator Addition [ Multiplication ]

        ( quiz, cmd ) =
            Quiz.init 12 vs ops
    in
    ( quiz, cmd )


subscriptions : Quiz.Model -> Sub Quiz.Message
subscriptions _ =
    Sub.none
