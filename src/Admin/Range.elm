module Admin.Range exposing (Message, Model, create, subscriptions, toRange, update, view)

import Admin.History as History exposing (History)
import DoubleSlider
import Expression exposing (Range(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import SingleSlider


type Model
    = FixedRange { slider : SingleSlider.Model, history : History RangeOption Model }
    | PositiveRange { slider : SingleSlider.Model, history : History RangeOption Model }
    | BetweenRange { slider : DoubleSlider.Model, history : History RangeOption Model }


push : Model -> Model -> Model
push historic current =
    let
        historicRangeOption =
            toRangeOption historic
    in
    case current of
        FixedRange ({ history } as data) ->
            FixedRange { data | history = History.push historicRangeOption historic history }

        PositiveRange ({ history } as data) ->
            PositiveRange { data | history = History.push historicRangeOption historic history }

        BetweenRange ({ history } as data) ->
            BetweenRange { data | history = History.push historicRangeOption historic history }


latest : RangeOption -> Model -> Maybe Model
latest option model =
    let
        aHistory =
            case model of
                FixedRange { history } ->
                    history

                PositiveRange { history } ->
                    history

                BetweenRange { history } ->
                    history
    in
    History.latest option aHistory


toRange : Model -> Range
toRange model =
    case model of
        FixedRange { slider } ->
            Fixed (floor slider.value)

        PositiveRange { slider } ->
            Positive (floor slider.value)

        BetweenRange { slider } ->
            Between (floor slider.lowValue) (floor slider.highValue)


type RangeOption
    = FixedOption
    | PositiveOption
    | BetweenOption


toRangeOption : Model -> RangeOption
toRangeOption model =
    case model of
        FixedRange _ ->
            FixedOption

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
        FixedOption ->
            defaultFixedRange

        PositiveOption ->
            defaultPositiveRange

        BetweenOption ->
            defaultBetweenRange


defaultFixedRange : Model
defaultFixedRange =
    FixedRange { slider = defaultSingleSlider, history = History.empty }


defaultPositiveRange : Model
defaultPositiveRange =
    PositiveRange { slider = defaultSingleSlider, history = History.empty }


defaultBetweenRange : Model
defaultBetweenRange =
    BetweenRange { slider = defaultDoubleSlider, history = History.empty }


defaultSingleSlider : SingleSlider.Model
defaultSingleSlider =
    let
        slider =
            SingleSlider.defaultModel
    in
    { slider | min = 1, max = 200, step = 1, value = 10 }


defaultDoubleSlider : DoubleSlider.Model
defaultDoubleSlider =
    let
        slider =
            DoubleSlider.defaultModel
    in
    { slider | min = 1, max = 200, step = 1, lowValue = 10, highValue = 20 }


type Message
    = FixedMessage SingleSlider.Msg
    | PositiveMessage SingleSlider.Msg
    | BetweenMessage DoubleSlider.Msg
    | Select RangeOption


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case ( message, model ) of
        ( FixedMessage msg, FixedRange ({ slider } as fixed) ) ->
            let
                ( nextSlider, cmd, _ ) =
                    SingleSlider.update msg slider
            in
            ( FixedRange { fixed | slider = nextSlider }, Cmd.map FixedMessage cmd )

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
    Html.div []
        [ viewRangeOption radiogroup FixedOption model
        , viewRangeOption radiogroup PositiveOption model
        , viewRangeOption radiogroup BetweenOption model
        , viewSlider model
        ]


viewRangeOption : String -> RangeOption -> Model -> Html Message
viewRangeOption radiogroup option model =
    let
        checked =
            option == toRangeOption model
    in
    Html.div []
        [ Html.input [ Attribute.type_ "radio", Attribute.name radiogroup, Attribute.checked checked, Event.onCheck <| \_ -> Select option ] []
        , Html.label [] [ Html.text <| rangeOptionToString option ]
        ]


rangeOptionToString : RangeOption -> String
rangeOptionToString option =
    case option of
        FixedOption ->
            "Fixed"

        PositiveOption ->
            "Positive"

        BetweenOption ->
            "Between"


viewSlider : Model -> Html Message
viewSlider model =
    case model of
        FixedRange { slider } ->
            SingleSlider.view slider
                |> Html.fromUnstyled
                |> Html.map FixedMessage

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
        [ Sub.map FixedMessage <| SingleSlider.subscriptions defaultSingleSlider
        , Sub.map PositiveMessage <| SingleSlider.subscriptions defaultSingleSlider
        , Sub.map BetweenMessage <| DoubleSlider.subscriptions defaultDoubleSlider
        ]
