module Admin.History exposing (History, empty, latest, push)


type History k v
    = History (List ( k, List v ))


empty : History k v
empty =
    History []


latest : k -> History k v -> Maybe v
latest target (History keys) =
    keys
        |> List.filter (\( key, _ ) -> key == target)
        |> List.head
        |> Maybe.andThen (\( _, history ) -> List.head history)


push : k -> v -> History k v -> History k v
push key value (History keys) =
    History <| updateKeys key value keys


updateKeys : k -> v -> List ( k, List v ) -> List ( k, List v )
updateKeys target value keys =
    let
        thereIsATargetKey =
            keys
                |> List.filter (\( key, _ ) -> key == target)
                |> List.head
                |> Maybe.map (\_ -> True)
                |> Maybe.withDefault False
    in
    if thereIsATargetKey then
        accumulatedUpdateKeys [] target value keys

    else
        ( target, [ value ] ) :: keys


accumulatedUpdateKeys : List ( k, List v ) -> k -> v -> List ( k, List v ) -> List ( k, List v )
accumulatedUpdateKeys accumulator target value remaining =
    case remaining of
        [] ->
            accumulator

        ( key, history ) :: tail ->
            let
                head =
                    if target == key then
                        ( key, value :: history )

                    else
                        ( key, history )
            in
            accumulatedUpdateKeys (head :: accumulator) target value tail
