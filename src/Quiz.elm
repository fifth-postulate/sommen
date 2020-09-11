module Quiz exposing (Message, Model, init, update, view)

import Css exposing (..)
import Expression exposing (Operator)
import Html.Attributes exposing (rows)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Question
import Random exposing (Generator)


init : Int -> Generator Int -> Generator Operator -> ( Model, Cmd Message )
init numberOfQuestions values operators =
    let
        ( question, cmd ) =
            Question.init values operators

        configuration =
            { valueGenerator = values
            , operatorGenerator = operators
            }
    in
    ( Quiz
        { currentQuestion = question
        , previousQuestions = []
        , nextQuestions = List.repeat (numberOfQuestions - 1) Nothing
        , configuration = configuration
        }
    , Cmd.map QuestionMessage cmd
    )


type Model
    = Quiz Data


type alias Data =
    { currentQuestion : Question.Model
    , previousQuestions : List Question.Model
    , nextQuestions : List (Maybe Question.Model)
    , configuration : Configuration
    }


type alias Configuration =
    { valueGenerator : Generator Int
    , operatorGenerator : Generator Operator
    }


next : Model -> ( Model, Cmd Message )
next ((Quiz data) as model) =
    case List.head data.nextQuestions of
        Just Nothing ->
            let
                ( question, cmd ) =
                    Question.init data.configuration.valueGenerator data.configuration.operatorGenerator
            in
            ( Quiz <| advance question data, Cmd.map QuestionMessage cmd )

        Just (Just question) ->
            ( Quiz <| advance question data, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


previous : Model -> Model
previous ((Quiz data) as model) =
    case List.head data.previousQuestions of
        Just question ->
            Quiz <| recede question data

        Nothing ->
            model


advance : Question.Model -> Data -> Data
advance question data =
    { data
        | currentQuestion = question
        , previousQuestions = data.currentQuestion :: data.previousQuestions
        , nextQuestions = List.tail data.nextQuestions |> Maybe.withDefault []
    }


recede : Question.Model -> Data -> Data
recede question data =
    { data
        | currentQuestion = question
        , previousQuestions = List.tail data.previousQuestions |> Maybe.withDefault []
        , nextQuestions = Just data.currentQuestion :: data.nextQuestions
    }


type Message
    = QuestionMessage Question.Message
    | Next
    | Previous


update : Message -> Model -> ( Model, Cmd Message )
update message ((Quiz data) as model) =
    case message of
        QuestionMessage msg ->
            let
                ( question, cmd ) =
                    Question.update msg data.currentQuestion
            in
            ( Quiz { data | currentQuestion = question }, Cmd.map QuestionMessage cmd )

        Next ->
            next model

        Previous ->
            ( previous model, Cmd.none )


view : Model -> Html Message
view (Quiz data) =
    Html.div [ Attribute.class "quiz" ]
        [ viewSummary data
        , Html.div
            [ Attribute.classList [ ( "quiz", True ), ( "current", True ) ]
            , Attribute.css
                [ displayFlex
                , flexDirection row
                , flexWrap noWrap
                , justifyContent center
                , alignItems center
                ]
            ]
            [ viewPrevious data
            , Html.map QuestionMessage <| Question.view data.currentQuestion
            , viewNext data
            ]
        ]


viewSummary : Data -> Html Message
viewSummary data =
    let
        toSummary candidate =
            candidate
                |> Maybe.map Question.summary
                |> Maybe.withDefault Question.defaultSummary

        content =
            [ Html.div
                [ Attribute.class "previous"
                , Attribute.css
                    [ displayFlex
                    , flexDirection rowReverse
                    , flexWrap noWrap
                    , justifyContent end
                    , alignItems center
                    ]
                ]
              <|
                List.map Question.summary data.previousQuestions
            , Html.div
                [ Attribute.class "current"
                , Attribute.css
                    [ displayFlex
                    , flexDirection row
                    , flexWrap noWrap
                    , justifyContent center
                    , alignItems center
                    ]
                ]
                [ Question.summary data.currentQuestion ]
            , Html.div
                [ Attribute.class "next"
                , Attribute.css
                    [ displayFlex
                    , flexDirection row
                    , flexWrap noWrap
                    , justifyContent start
                    , alignItems center
                    ]
                ]
              <|
                List.map toSummary data.nextQuestions
            ]
    in
    Html.div
        [ Attribute.class "summary"
        , Attribute.css
            [ displayFlex
            , flexDirection row
            , flexWrap noWrap
            , justifyContent center
            , alignItems center
            ]
        ]
        content


viewPrevious : Data -> Html Message
viewPrevious { previousQuestions } =
    case List.head previousQuestions of
        Just _ ->
            Html.div [ Event.onClick Previous ] [ Html.text "⏴" ]

        Nothing ->
            Html.div [] []


viewNext : Data -> Html Message
viewNext { nextQuestions } =
    case List.head nextQuestions of
        Just _ ->
            Html.div [ Event.onClick Next ] [ Html.text "⏵" ]

        Nothing ->
            Html.div [] []
