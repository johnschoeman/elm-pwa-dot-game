module Levels exposing (Model, Msg, init, update, view)

import Html exposing (Html, a, button, div, h1, img, li, text, ul)
import Html.Attributes exposing (class, src)



---- MODEL ----


type alias Model =
    {}


init : Model
init =
    {}



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



---- VIEW ----


view : Html Msg
view =
    div []
        [ ul [] (List.map (\level -> li [] [ text ("level " ++ String.fromInt level) ]) (List.range 1 15))
        ]
