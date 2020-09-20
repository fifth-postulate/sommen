module Expression exposing (Expression, Operator(..), Range(..), decodeExpression, decodeOperator, decodeRange, decodeValue, encodeExpression, encodeOperator, encodeRange, encodeValue, eval, expression, operator, operatorToString, value, view)

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Random exposing (Generator)


type Expression
    = Expression Int Operator Int


eval : Expression -> Int
eval (Expression left op right) =
    lookup op left right


lookup : Operator -> Int -> Int -> Int
lookup op =
    case op of
        Addition ->
            (+)

        Subtraction ->
            (-)

        Multiplication ->
            (*)


type Operator
    = Addition
    | Subtraction
    | Multiplication


type Range
    = Positive Int
    | Between Int Int


expression : Generator Int -> Generator Operator -> Generator Int -> Generator Expression
expression =
    Random.map3 Expression


value : Range -> Generator Int
value range =
    case range of
        Positive maximum ->
            Random.int 0 maximum

        Between minimum maximum ->
            Random.int minimum maximum


operator : Operator -> List Operator -> Generator Operator
operator =
    Random.uniform


view : Expression -> Html msg
view (Expression left op right) =
    Html.div
        [ Attribute.class "expression"
        , Attribute.css
            [ display inlineBlock
            ]
        ]
        [ viewValue left
        , viewOperator op
        , viewValue right
        ]


viewValue : Int -> Html msg
viewValue n =
    viewIntegerValue n


viewIntegerValue : Int -> Html msg
viewIntegerValue n =
    Html.span [ Attribute.class "value" ] [ Html.text <| String.fromInt n ]


viewOperator : Operator -> Html msg
viewOperator op =
    Html.span [ Attribute.class "operator" ] [ Html.text <| operatorToString op ]


operatorToString : Operator -> String
operatorToString op =
    case op of
        Addition ->
            "+"

        Subtraction ->
            "-"

        Multiplication ->
            "⨉"


encodeExpression : Expression -> Encode.Value
encodeExpression (Expression left op right) =
    Encode.object
        [ ( "tag", Encode.string "Expression" )
        , ( "type", Encode.string "Expression" )
        , ( "left", encodeValue left )
        , ( "operator", encodeOperator op )
        , ( "right", encodeValue right )
        ]


decodeExpression : Decoder Expression
decodeExpression =
    let
        decodeExpressionExpression =
            Decode.map3 Expression
                (Decode.field "left" decodeValue)
                (Decode.field "operator" decodeOperator)
                (Decode.field "right" decodeValue)

        decodeExpressionType aType =
            case aType of
                "Expression" ->
                    decodeExpressionExpression

                _ ->
                    Decode.fail <| "unrecognized Expression type \"" ++ aType ++ "\""

        typeSelector =
            decodeType
                |> Decode.andThen decodeExpressionType
    in
    decodeTag
        |> Decode.andThen (guardedDecoder "Expression" typeSelector)


decodeTag : Decoder String
decodeTag =
    Decode.field "tag" Decode.string


decodeType : Decoder String
decodeType =
    Decode.field "type" Decode.string


guardedDecoder : String -> Decoder a -> String -> Decoder a
guardedDecoder target decoder candidate =
    if target == candidate then
        decoder

    else
        Decode.fail <| "expected target \"" ++ target ++ "\" but found \"" ++ candidate ++ "\""


encodeOperator : Operator -> Encode.Value
encodeOperator op =
    Encode.string <| operatorToString op


decodeOperator : Decoder Operator
decodeOperator =
    let
        operatorSelector op =
            case op of
                "+" ->
                    Decode.succeed Addition

                "-" ->
                    Decode.succeed Subtraction

                "⨉" ->
                    Decode.succeed Multiplication

                _ ->
                    Decode.fail <| "unknown operator \"" ++ op ++ "\""
    in
    Decode.string
        |> Decode.andThen operatorSelector


encodeValue : Int -> Encode.Value
encodeValue n =
    Encode.object
        [ ( "tag", Encode.string "Value" )
        , ( "type", Encode.string "Integer" )
        , ( "value", Encode.int n )
        ]


decodeValue : Decoder Int
decodeValue =
    let
        decodeIntegerValue =
            Decode.field "value" Decode.int

        decodeValueType aType =
            case aType of
                "Integer" ->
                    decodeIntegerValue

                _ ->
                    Decode.fail <| "unrecognized Value type \"" ++ aType ++ "\""

        typeSelector =
            decodeType
                |> Decode.andThen decodeValueType
    in
    decodeTag
        |> Decode.andThen (guardedDecoder "Value" typeSelector)


encodeRange : Range -> Encode.Value
encodeRange range =
    case range of
        Positive n ->
            Encode.object
                [ ( "tag", Encode.string "Range" )
                , ( "type", Encode.string "Positive" )
                , ( "maximum", Encode.int n )
                ]

        Between minimum maximum ->
            Encode.object
                [ ( "tag", Encode.string "Range" )
                , ( "type", Encode.string "Between" )
                , ( "minimum", Encode.int minimum )
                , ( "maximum", Encode.int maximum )
                ]


decodeRange : Decoder Range
decodeRange =
    let
        decodePositiveRange =
            Decode.map Positive <|
                Decode.field "maximum" Decode.int

        decodeBetweenRange =
            Decode.map2 Between
                (Decode.field "minimum" Decode.int)
                (Decode.field "maximum" Decode.int)

        decodeRangeType aType =
            case aType of
                "Positive" ->
                    decodePositiveRange

                "Between" ->
                    decodeBetweenRange

                _ ->
                    Decode.fail <| "unrecognized Value type \"" ++ aType ++ "\""

        typeSelector =
            decodeType
                |> Decode.andThen decodeRangeType
    in
    decodeTag
        |> Decode.andThen (guardedDecoder "Range" typeSelector)
