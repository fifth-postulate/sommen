module Sommen exposing (main)

import Base64
import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Expression exposing (Operator(..), Range(..))
import Html.Styled as Html exposing (Html, a, i)
import Json.Decode as Json
import Quiz
import Url exposing (Url)
import Url.Parser as Parser
import Url.Parser.Query as Query


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
        toModel ( quiz, cmd ) =
            ( Initialized quiz, Cmd.map QuizMessage cmd )
    in
    url
        |> Parser.parse (Parser.query (Query.string "quiz"))
        |> Maybe.andThen identity
        |> Result.fromMaybe CouldNotRetrieveQuiz
        |> Result.andThen (Base64.decode >> Result.mapError Base64Decode)
        |> Result.andThen (Json.decodeString Quiz.decodeDescription >> Result.mapError JsonDecode)
        |> Result.map Quiz.init
        |> Result.map toModel
        |> Result.mapError CouldNotInitializeQuiz
        |> Result.mapError (\m -> ( m, Cmd.none ))
        |> unwrap


unwrap : Result a a -> a
unwrap result =
    case result of
        Ok value ->
            value

        Err value ->
            value


type Model
    = CouldNotInitializeQuiz Problem
    | Initialized Quiz.Model


type Problem
    = CouldNotRetrieveQuiz
    | Base64Decode String
    | JsonDecode Json.Error


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

        ( _, _ ) ->
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

                CouldNotInitializeQuiz problem ->
                    whoops problem
    in
    { title = "Sommen"
    , body = [ Html.toUnstyled html ]
    }


whoops : Problem -> Html msg
whoops problem =
    let
        explanation =
            case problem of
                CouldNotRetrieveQuiz ->
                    "no quiz"

                Base64Decode reason ->
                    "base64 decode: " ++ reason

                JsonDecode error ->
                    "JSON decode: " ++ Json.errorToString error
    in
    Html.div []
        [ Html.h1 [] [ Html.text "Sommen" ]
        , Html.p [] [ Html.text "Ik kon geen sommen maken voor jou." ]
        , Html.p [] [ Html.text explanation ]
        ]


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none
