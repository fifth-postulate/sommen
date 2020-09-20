module Admin.Operator exposing (Message, Model, create, toPair, update, view)

import Expression exposing (Operator(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event


type Model
    = Operators
        { main : Operator
        , rest : List Operator
        }


create : Operator -> Model
create op =
    Operators { main = op, rest = [] }


toPair : Model -> ( Operator, List Operator )
toPair (Operators { main, rest }) =
    ( main, rest )


type Message
    = Remove Operator
    | Add Operator


update : Message -> Model -> ( Model, Cmd Message )
update message ((Operators ({ main, rest } as operators)) as model) =
    let
        nextModel =
            case message of
                Remove op ->
                    case ( main, rest ) of
                        ( m, h :: hs ) ->
                            if m == op then
                                Operators { operators | main = h, rest = hs }

                            else
                                Operators { operators | rest = remove op rest }

                        ( _, [] ) ->
                            model

                Add op ->
                    if contains op model then
                        model

                    else
                        Operators { operators | rest = op :: rest }
    in
    ( nextModel, Cmd.none )


remove : a -> List a -> List a
remove target candidates =
    tail_recursive_remove [] target candidates


tail_recursive_remove : List a -> a -> List a -> List a
tail_recursive_remove accumulator target candidates =
    case candidates of
        [] ->
            List.reverse accumulator

        x :: xs ->
            let
                nextAccumulator =
                    if x /= target then
                        x :: accumulator

                    else
                        accumulator
            in
            tail_recursive_remove nextAccumulator target xs


contains : Operator -> Model -> Bool
contains op (Operators { main, rest }) =
    main == op || List.member op rest


view : Model -> Html Message
view model =
    let
        content =
            [ Addition, Subtraction, Multiplication ]
                |> List.map (viewOperator model)
    in
    Html.div [] content


viewOperator : Model -> Operator -> Html Message
viewOperator model op =
    let
        present =
            contains op model

        message checked =
            if checked then
                Add op

            else
                Remove op
    in
    Html.div []
        [ Html.input [ Attribute.type_ "checkbox", Attribute.checked present, Event.onCheck message ] []
        , Html.label [] [ Html.text <| Expression.operatorToString op ]
        ]
