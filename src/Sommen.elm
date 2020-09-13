module Sommen exposing (..)

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Expression exposing (Operator(..), Range(..))
import Html.Styled as Html
import Quiz
import Url exposing (Url)


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = \_ -> DoNothing
        , onUrlRequest = \_ -> DoNothing
        }


init : () -> Url -> Key -> ( Model, Cmd Message )
init _ url _ =
    let
        description =
            { numberOfQuestions = 12
            , valueRange = Between 10 50
            , operators = ( Addition, [ Multiplication ] )
            }

        ( quiz, cmd ) =
            Quiz.init description
    in
    ( Initialized quiz, Cmd.map QuizMessage cmd )


type Model
    = Initialized Quiz.Model


type Message
    = QuizMessage Quiz.Message
    | DoNothing


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case ( message, model ) of
        ( QuizMessage msg, Initialized quiz ) ->
            let
                ( nextQuiz, cmd ) =
                    Quiz.update msg quiz
            in
            ( Initialized nextQuiz, Cmd.map QuizMessage cmd )

        ( DoNothing, _ ) ->
            ( model, Cmd.none )


view : Model -> Document Message
view model =
    let
        html =
            case model of
                Initialized quiz ->
                    quiz
                        |> Quiz.view
                        |> Html.map QuizMessage
                        |> Html.toUnstyled
    in
    { title = "Sommen"
    , body = [ html ]
    }


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none
