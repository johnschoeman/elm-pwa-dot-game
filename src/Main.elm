module Main exposing (..)

import Browser
import Game
import Html exposing (Html, a, button, div, h1, img, li, text, ul)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Levels



---- MODEL ----


type Screen
    = Game
    | Levels


type alias Model =
    { currentScreen : Screen
    , game : Game.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { currentScreen = Game, game = Game.init }, Cmd.none )



---- UPDATE ----


type Msg
    = ChangeScreen Screen
    | GotGameMsg Game.Msg
    | GotLevelsMsg Levels.Msg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeScreen screen ->
            ( { model | currentScreen = screen }, Cmd.none )

        GotGameMsg subMsg ->
            ( { model | game = Game.update subMsg model.game }, Cmd.none )

        GotLevelsMsg subMsg ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        body =
            case model.currentScreen of
                Game ->
                    Html.map (\gameMsg -> GotGameMsg gameMsg) (Game.view model.game)

                Levels ->
                    Html.map (\levelsMsg -> GotLevelsMsg levelsMsg) Levels.view
    in
    div [ class "bg-gray-100" ]
        [ div [ class "p-8" ]
            [ header
            , body
            ]
        ]


header : Html Msg
header =
    div [ class "border-b-2 border-gray-800 py-8" ]
        [ h1 [ class "text-2xl text-gray-800 " ] [ text "Elm PWA Dot Game" ]
        , navigation
        ]


navigation : Html Msg
navigation =
    let
        buttonStyle =
            "bg-blue-500 text-white font-bold py-2 px-4 mr-8 rounded"
    in
    div [ class "mt-4" ]
        [ a [ class buttonStyle, onClick (ChangeScreen Game) ] [ text "Game" ]
        , a [ class buttonStyle, onClick (ChangeScreen Levels) ] [ text "Levels" ]
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
