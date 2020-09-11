module Question exposing (Message, Model, init, update, view)

import Expression exposing (Expression, Operator, expression)
import Html exposing (Html)
import Random exposing (Generator)


init : Generator Int -> Generator Operator -> ( Model, Cmd Message )
init vs ops =
    let
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
update message _ =
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
