module Admin.Number exposing (Message, Model, create, subscriptions, update, value, view)

import Html.Styled as Html exposing (Html)
import SingleSlider


type Model
    = Slider SingleSlider.Model


create : Model
create =
    let
        defaultSlider =
            SingleSlider.defaultModel
    in
    Slider { defaultSlider | min = 1, max = 50, step = 1, value = 10 }


value : Model -> Int
value (Slider slider) =
    slider.value
        |> floor


type Message
    = SliderMessage SingleSlider.Msg


update : Message -> Model -> ( Model, Cmd Message )
update (SliderMessage msg) (Slider model) =
    let
        ( nextModel, cmd, _ ) =
            SingleSlider.update msg model
    in
    ( Slider nextModel, Cmd.map SliderMessage cmd )


view : Model -> Html Message
view (Slider model) =
    SingleSlider.view model
        |> Html.fromUnstyled
        |> Html.map SliderMessage


subscriptions : Model -> Sub Message
subscriptions (Slider model) =
    Sub.map SliderMessage <| SingleSlider.subscriptions model
