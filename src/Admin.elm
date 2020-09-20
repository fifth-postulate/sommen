module Admin exposing (..)

import Admin.Operator as OperatorAdmin
import Base64
import Browser
import Expression exposing (Operator(..), Range(..))
import Html.Styled as Html exposing (Html)
import Json.Encode as Encode
import Quiz


main =
    Browser.element
        { init = init
        , view = view >> Html.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Message )
init _ =
    ( { operators = OperatorAdmin.create Addition }, Cmd.none )


type alias Model =
    { operators : OperatorAdmin.Model
    }


type Message
    = OperatorMessage OperatorAdmin.Message


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        OperatorMessage msg ->
            let
                ( nextModel, cmd ) =
                    OperatorAdmin.update msg model.operators
            in
            ( { model | operators = nextModel }, Cmd.map OperatorMessage cmd )


view : Model -> Html Message
view model =
    let
        description =
            { numberOfQuestions = 12
            , valueRange = Between 10 20
            , operators = OperatorAdmin.toPair model.operators
            }

        representation =
            description
                |> Quiz.encodeDescription
                |> Encode.encode 0
                |> Base64.encode
    in
    Html.div []
        [ Html.map OperatorMessage <| OperatorAdmin.view model.operators
        , Html.div []
            [ Html.span [] [ Html.text representation ]
            ]
        ]


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none
