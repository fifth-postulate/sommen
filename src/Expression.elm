module Expression exposing (Expression, Operator(..), Range(..), eval, expression, operator, value, view)

import Html exposing (Html)
import Random exposing (Generator)


type Expression
    = Expression Int Operator Int


eval : Expression -> Maybe Int
eval (Expression left op right) =
    lookup op left right


lookup : Operator -> Int -> Int -> Maybe Int
lookup op l r =
    case op of
        Addition ->
            Just <| l + r

        Subtraction ->
            Just <| l - r

        Multiplication ->
            Just <| l * r


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
    Html.div []
        [ viewValue left
        , viewOperator op
        , viewValue right
        ]


viewValue : Int -> Html msg
viewValue n =
    viewIntegerValue n


viewIntegerValue : Int -> Html msg
viewIntegerValue n =
    Html.span [] [ Html.text <| String.fromInt n ]


viewOperator : Operator -> Html msg
viewOperator op =
    let
        symbol =
            case op of
                Addition ->
                    "+"

                Subtraction ->
                    "-"

                Multiplication ->
                    "⨉"
    in
    Html.span [] [ Html.text symbol ]
