module Admin exposing (..)

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
    ( {}, Cmd.none )


type alias Model =
    {}


type Message
    = DoNothing


update : Message -> Model -> ( Model, Cmd Message )
update _ model =
    ( model, Cmd.none )


view : Model -> Html Message
view _ =
    let
        description =
            { numberOfQuestions = 12
            , valueRange = Between 10 20
            , operators = ( Addition, [ Multiplication ] )
            }

        representation =
            description
                |> Quiz.encodeDescription
                |> Encode.encode 0
                |> Base64.encode
    in
    Html.text representation


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none
