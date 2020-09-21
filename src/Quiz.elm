module Quiz exposing (Description, Message, Model, decodeDescription, encodeDescription, init, update, view)

import Css exposing (..)
import Expression exposing (Operator, Range, operator, value)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import KeyEvent exposing (onPageDown)
import Question
import Random exposing (Generator)


init : Description -> ( Model, Cmd Message )
init description =
    let
        configuration =
            configurationFrom description

        ( question, cmd ) =
            Question.init configuration.leftGenerator configuration.operatorGenerator configuration.rightGenerator
    in
    ( Quiz
        { currentQuestion = question
        , previousQuestions = []
        , nextQuestions = List.repeat (description.numberOfQuestions - 1) Nothing
        , configuration = configuration
        }
    , Cmd.map QuestionMessage cmd
    )


type alias Description =
    { numberOfQuestions : Int
    , leftRange : Range
    , operators : ( Operator, List Operator )
    , rightRange : Range
    }


encodeDescription : Description -> Encode.Value
encodeDescription description =
    let
        encodeOperators ( main, rest ) =
            Encode.object
                [ ( "main", Expression.encodeOperator main )
                , ( "rest", Encode.list Expression.encodeOperator rest )
                ]
    in
    Encode.object
        [ ( "numberOfQuestions", Encode.int description.numberOfQuestions )
        , ( "leftRange", Expression.encodeRange description.leftRange )
        , ( "operators", encodeOperators description.operators )
        , ( "rightRange", Expression.encodeRange description.rightRange )
        ]


decodeDescription : Decoder Description
decodeDescription =
    let
        decodeOperators : Decoder ( Operator, List Operator )
        decodeOperators =
            Decode.map2 Tuple.pair
                (Decode.field "main" Expression.decodeOperator)
                (Decode.field "rest" <| Decode.list Expression.decodeOperator)
    in
    Decode.map4 Description
        (Decode.field "numberOfQuestions" Decode.int)
        (Decode.field "leftRange" Expression.decodeRange)
        (Decode.field "operators" decodeOperators)
        (Decode.field "rightRange" Expression.decodeRange)


type Model
    = Quiz Data


type alias Data =
    { currentQuestion : Question.Model
    , previousQuestions : List Question.Model
    , nextQuestions : List (Maybe Question.Model)
    , configuration : Configuration
    }


type alias Configuration =
    { leftGenerator : Generator Int
    , operatorGenerator : Generator Operator
    , rightGenerator : Generator Int
    }


configurationFrom : Description -> Configuration
configurationFrom description =
    let
        ls =
            value <| description.leftRange

        ops =
            uncurry operator description.operators

        rs =
            value <| description.rightRange
    in
    { leftGenerator = ls
    , operatorGenerator = ops
    , rightGenerator = rs
    }


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


next : Model -> ( Model, Cmd Message )
next ((Quiz data) as model) =
    case List.head data.nextQuestions of
        Just Nothing ->
            let
                ( question, cmd ) =
                    Question.init data.configuration.leftGenerator data.configuration.operatorGenerator data.configuration.rightGenerator
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
    | DoNothing


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

        DoNothing ->
            ( model, Cmd.none )


view : Model -> Html Message
view (Quiz data) =
    Html.div [ Attribute.class "quiz", onPageDown Next DoNothing ]
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
                    , transform (scale 1.2)
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
            Html.div [ Attribute.css [ fontSize (px 50) ], Event.onClick Previous ] [ Html.text "⏴" ]

        Nothing ->
            Html.div [] []


viewNext : Data -> Html Message
viewNext { nextQuestions } =
    case List.head nextQuestions of
        Just _ ->
            Html.div [ Attribute.css [ fontSize (px 50) ], Event.onClick Next ] [ Html.text "⏵" ]

        Nothing ->
            Html.div [] []
