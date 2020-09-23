module Admin.Range exposing (Message, Model, create, subscriptions, toRange, update, view)

import Admin.History as History exposing (History)
import DoubleSlider
import Expression exposing (Range(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import SingleSlider


type Model
    = PositiveRange { slider : SingleSlider.Model, history : History RangeOption Model }
    | BetweenRange { slider : DoubleSlider.Model, history : History RangeOption Model }


push : Model -> Model -> Model
push historic current =
    let
        historicRangeOption =
            toRangeOption historic
    in
    case current of
        PositiveRange ({ history } as data) ->
            PositiveRange { data | history = History.push historicRangeOption historic history }

        BetweenRange ({ history } as data) ->
            BetweenRange { data | history = History.push historicRangeOption historic history }


latest : RangeOption -> Model -> Maybe Model
latest option model =
    let
        aHistory =
            case model of
                PositiveRange { history } ->
                    history

                BetweenRange { history } ->
                    history
    in
    History.latest option aHistory


toRange : Model -> Range
toRange model =
    case model of
        PositiveRange { slider } ->
            Positive (floor slider.value)

        BetweenRange { slider } ->
            Between (floor slider.lowValue) (floor slider.highValue)


type RangeOption
    = PositiveOption
    | BetweenOption


toRangeOption : Model -> RangeOption
toRangeOption model =
    case model of
        PositiveRange _ ->
            PositiveOption

        BetweenRange _ ->
            BetweenOption


create : Model
create =
    default PositiveOption


default : RangeOption -> Model
default option =
    case option of
        PositiveOption ->
            defaultPositiveRange

        BetweenOption ->
            defaultBetweenRange


defaultPositiveRange : Model
defaultPositiveRange =
    PositiveRange { slider = defaultPositiveSlider, history = History.empty }


defaultPositiveSlider : SingleSlider.Model
defaultPositiveSlider =
    let
        defaultSingleSlider =
            SingleSlider.defaultModel
    in
    { defaultSingleSlider | min = 1, max = 200, step = 1, value = 10 }


defaultBetweenRange : Model
defaultBetweenRange =
    BetweenRange { slider = defaultBetweenSlider, history = History.empty }


defaultBetweenSlider : DoubleSlider.Model
defaultBetweenSlider =
    let
        defaultDoubleSlider =
            DoubleSlider.defaultModel
    in
    { defaultDoubleSlider | min = 1, max = 200, step = 1, lowValue = 10, highValue = 20 }


type Message
    = PositiveMessage SingleSlider.Msg
    | BetweenMessage DoubleSlider.Msg
    | Select RangeOption


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case ( message, model ) of
        ( PositiveMessage msg, PositiveRange ({ slider } as positive) ) ->
            let
                ( nextSlider, cmd, _ ) =
                    SingleSlider.update msg slider
            in
            ( PositiveRange { positive | slider = nextSlider }, Cmd.map PositiveMessage cmd )

        ( BetweenMessage msg, BetweenRange ({ slider } as between) ) ->
            let
                ( nextSlider, cmd, _ ) =
                    DoubleSlider.update msg slider
            in
            ( BetweenRange { between | slider = nextSlider }, Cmd.map BetweenMessage cmd )

        ( Select option, _ ) ->
            let
                nextModel =
                    model
                        |> latest option
                        |> Maybe.withDefault (default option)
            in
            ( push model nextModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : String -> Model -> Html Message
view radiogroup model =
    let
        positiveChecked =
            case model of
                PositiveRange _ ->
                    True

                _ ->
                    False

        betweenChecked =
            case model of
                BetweenRange _ ->
                    True

                _ ->
                    False

        checked option _ =
            Select option
    in
    Html.div []
        [ Html.input [ Attribute.type_ "radio", Attribute.name radiogroup, Attribute.checked positiveChecked, Event.onCheck <| checked PositiveOption ] []
        , Html.label [] [ Html.text "Positive" ]
        , Html.input [ Attribute.type_ "radio", Attribute.name radiogroup, Attribute.checked betweenChecked, Event.onCheck <| checked BetweenOption ] []
        , Html.label [] [ Html.text "Between" ]
        , viewSlider model
        ]


viewSlider : Model -> Html Message
viewSlider model =
    case model of
        PositiveRange { slider } ->
            SingleSlider.view slider
                |> Html.fromUnstyled
                |> Html.map PositiveMessage

        BetweenRange { slider } ->
            DoubleSlider.fallbackView slider
                |> Html.fromUnstyled
                |> Html.map BetweenMessage


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.batch
        [ Sub.map PositiveMessage <| SingleSlider.subscriptions defaultPositiveSlider
        , Sub.map BetweenMessage <| DoubleSlider.subscriptions defaultBetweenSlider
        ]
