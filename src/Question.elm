module Question exposing (Message, Model, defaultSummary, init, summary, update, view)

import Expression exposing (Expression, Operator, expression)
import Html exposing (Html)
import Html.Attributes as Attribute
import Html.Events as Event
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
    | Question Data


type alias Data =
    { expression : Expression
    , answer : Maybe Int
    , answerInput : String
    , status : Status
    }


type Status
    = UnChecked
    | Correct
    | Incorrect


status : Model -> Status
status model =
    case model of
        CreatingQuestion ->
            UnChecked

        Question data ->
            data.status


toStatus : Bool -> Status
toStatus correct =
    if correct then
        Correct

    else
        Incorrect


statusToString : Status -> String
statusToString aStatus =
    case aStatus of
        UnChecked ->
            "?"

        Correct ->
            "✔"

        Incorrect ->
            "❌"


create : Expression -> Data
create expr =
    { expression = expr
    , answer = Nothing
    , answerInput = ""
    , status = UnChecked
    }


type Message
    = ExpressionReceived Expression
    | InputChanged String
    | Checked


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        ExpressionReceived e ->
            ( Question <| create e, Cmd.none )

        InputChanged input ->
            ( updateData input model, Cmd.none )

        Checked ->
            ( check model, Cmd.none )


updateData : String -> Model -> Model
updateData input model =
    case model of
        CreatingQuestion ->
            CreatingQuestion

        Question data ->
            Question { data | answerInput = input, answer = String.toInt input }


check : Model -> Model
check model =
    case model of
        CreatingQuestion ->
            CreatingQuestion

        Question data ->
            case ( data.status, data.answer ) of
                ( UnChecked, Just n ) ->
                    let
                        aStatus =
                            data.expression
                                |> Expression.eval
                                |> (==) n
                                |> toStatus
                    in
                    Question { data | status = aStatus }

                _ ->
                    Question data


view : Model -> Html Message
view model =
    case model of
        CreatingQuestion ->
            waitForIt

        Question data ->
            viewData data


waitForIt : Html msg
waitForIt =
    Html.text "creating a question"


viewData : Data -> Html Message
viewData data =
    let
        hasAnswer =
            data.answer
                |> Maybe.map (\_ -> True)
                |> Maybe.withDefault False

        answerUnchecked =
            data.status == UnChecked

        disabled =
            not hasAnswer || not answerUnchecked
    in
    Html.div []
        [ Expression.view data.expression
        , Html.span [] [ Html.text "=" ]
        , Html.input [ Attribute.type_ "input", Event.onInput InputChanged ] []
        , viewStatus data.status
        , Html.button [ Attribute.disabled disabled, Event.onClick Checked ] [ Html.text "Check" ]
        ]


viewStatus : Status -> Html msg
viewStatus aStatus =
    Html.span [] [ Html.text <| statusToString aStatus ]


summary : Model -> Html msg
summary model =
    model
        |> status
        |> statusToString
        |> summaryContent


summaryContent : String -> Html msg
summaryContent content =
    Html.div [] [ Html.text content ]


defaultSummary : Html msg
defaultSummary =
    summaryContent <| statusToString UnChecked
