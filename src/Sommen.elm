module Sommen exposing (..)

import Browser
import Expression exposing (Expression, Operator(..), Range(..), expression, operator, value)
import Html exposing (Html)
import Question
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

        ( question, cmd ) =
            Question.init vs ops
    in
    ( Quiz question, Cmd.map QuestionMessage cmd )


type Model
    = Quiz Question.Model


type Message
    = QuestionMessage Question.Message


update : Message -> Model -> ( Model, Cmd Message )
update message (Quiz questionModel) =
    case message of
        QuestionMessage msg ->
            let
                ( nextQuestionModel, cmd ) =
                    Question.update msg questionModel
            in
            ( Quiz nextQuestionModel, Cmd.map QuestionMessage cmd )


view : Model -> Html Message
view model =
    case model of
        Quiz questionModel ->
            Question.view questionModel
                |> Html.map QuestionMessage


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none
