module Sommen exposing (..)

import Browser
import Expression exposing (Expression, Operator(..), Range(..), expression, operator, value)
import Html exposing (Html)
import Random


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Message )
init _ =
    let
        vs =
            value <| Between 10 50

        ops =
            operator Addition [ Multiplication ]

        generator =
            expression vs ops vs
    in
    ( CreatingQuestion, Random.generate ExpressionReceived generator )


type Model
    = CreatingQuestion
    | Question Expression


type Message
    = ExpressionReceived Expression


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        ExpressionReceived e ->
            ( Question e, Cmd.none )


view : Model -> Html Message
view model =
    case model of
        CreatingQuestion ->
            waitForIt

        Question e ->
            Expression.view e


waitForIt : Html msg
waitForIt =
    Html.text "creating a question"


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none
