module Screen.Levels.Main exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Html exposing (Html, text)
import Session exposing (WithSession)


type alias Model =
    WithSession {}


type Msg
    = NoOp


init : Session.Model -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


view : Model -> Html Msg
view _ =
    text "Levels"
