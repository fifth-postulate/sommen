module KeyEvent exposing (onEnter, onPageDown)

import Html.Styled exposing (Attribute)
import Html.Styled.Events as Event
import Json.Decode as Decode


onKey : Int -> msg -> msg -> Attribute msg
onKey keyCode messageOnKey alternative =
    let
        toMessage n =
            if n == keyCode then
                messageOnKey

            else
                alternative

        decoder =
            Event.keyCode
                |> Decode.map toMessage
    in
    Event.on "keyup" decoder


onEnter : msg -> msg -> Attribute msg
onEnter =
    onKey 13


onPageDown : msg -> msg -> Attribute msg
onPageDown =
    onKey 34
