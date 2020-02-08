module Main exposing (..)

import Browser
import Html exposing (Html, button, text, div, h1, img)
import Html.Events exposing (onClick)
import Html.Attributes exposing (src, class)


---- MODEL ----


type alias Model =
    {
      count: Int
    }


init : ( Model, Cmd Msg )
init =
    ( { count =  0 }, Cmd.none )



---- UPDATE ----


type Msg
    = Increment | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Increment -> 
      ({ model | count = model.count + 1}, Cmd.none)
    Decrement ->
      ({ model | count = model.count - 1}, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [
        h1 [] [ text "Elm PWA Push Notification" ]
        , button [class "text-gray-600", onClick Decrement] [ text "-" ]
        , text (String.fromInt model.count)
        , button [onClick Increment] [ text "+" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
