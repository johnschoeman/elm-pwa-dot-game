module Main exposing (..)

import Browser
import Html exposing (Html, a, button, div, h1, img, li, text, ul)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Level exposing (Level)
import Screen.Game as Game
import Screen.Levels as Levels



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
    = GoToGame Level
    | GoToLevels
    | GotGameMsg Game.Msg
    | GotLevelsMsg Levels.Msg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotGameMsg subMsg ->
            ( { model | game = Game.update subMsg model.game }, Cmd.none )

        GotLevelsMsg subMsg ->
            ( model, Cmd.none )

        GoToGame level ->
            ( { model | currentScreen = Game, game = Game.updateLevel level model.game }, Cmd.none )

        GoToLevels ->
            ( { model | currentScreen = Levels }, Cmd.none )

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
                    Levels.view goToGameCallback
    in
    div [ class "bg-gray-100" ]
        [ div [ class "p-8" ]
            [ header
            , body
            ]
        ]


goToGameCallback : Level -> Html.Attribute Msg
goToGameCallback level =
    onClick (GoToGame level)


header : Html Msg
header =
    div [ class "border-b-2 border-gray-800 py-8" ]
        [ h1 [ class "text-2xl text-gray-800 " ] [ text "Elm PWA Dot Game" ]
        , navigation
        ]


navigation : Html Msg
navigation =
    div [ class "mt-4" ]
        [ navigateToGameButton
        , navigateToLevelsButton
        ]


buttonStyle : String
buttonStyle =
    "bg-blue-500 text-white font-bold py-2 px-4 mr-8 rounded"


navigateToGameButton : Html Msg
navigateToGameButton =
    button [ onClick (GoToGame Level.level1) ] [ text "Game" ]


navigateToLevelsButton : Html Msg
navigateToLevelsButton =
    button [ onClick GoToLevels ] [ text "Levels" ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
