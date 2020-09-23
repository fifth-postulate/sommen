module Sommen exposing (..)

import Base64
import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Expression exposing (Operator(..), Range(..))
import Html.Styled as Html exposing (Html)
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
        |> Maybe.andThen (Base64.decode >> Result.toMaybe)
        |> Maybe.andThen (Json.decodeString Quiz.decodeDescription >> Result.toMaybe)
        |> Maybe.map Quiz.init
        |> Maybe.map toModel
        |> Maybe.withDefault ( CouldNotInitializeQuiz, Cmd.none )


queryToDict : String -> Dict String String
queryToDict input =
    let
        splitAtIndex : String -> Int -> ( String, String )
        splitAtIndex word n =
            ( String.left n word, String.dropLeft (n + 1) word )

        toPair query =
            query
                |> String.indices "="
                |> List.head
                |> Maybe.map (splitAtIndex query)

        fill query dict =
            query
                |> Maybe.map (\( k, v ) -> Dict.insert k v dict)
                |> Maybe.withDefault dict
    in
    input
        |> String.split "&"
        |> List.map toPair
        |> List.foldl fill Dict.empty


type Model
    = CouldNotInitializeQuiz
    | Initialized Quiz.Model


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

                CouldNotInitializeQuiz ->
                    whoops
    in
    { title = "Sommen"
    , body = [ Html.toUnstyled html ]
    }


whoops : Html msg
whoops =
    Html.div []
        [ Html.h1 [] [ Html.text "Sommen" ]
        , Html.p [] [ Html.text "Ik kon geen sommen maken voor jou." ]
        ]


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none
