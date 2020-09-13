module Sommen exposing (..)

import Browser
import Expression exposing (Operator(..), Range(..))
import Html.Styled as Html
import Quiz


main =
    Browser.element
        { init = init
        , view = Quiz.view >> Html.toUnstyled
        , update = Quiz.update
        , subscriptions = subscriptions
        }


init : () -> ( Quiz.Model, Cmd Quiz.Message )
init _ =
    let
        description =
            { numberOfQuestions = 12
            , valueRange = Between 10 50
            , operators = ( Addition, [ Multiplication ] )
            }

        ( quiz, cmd ) =
            Quiz.init description
    in
    ( quiz, cmd )


subscriptions : Quiz.Model -> Sub Quiz.Message
subscriptions _ =
    Sub.none
