port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
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
    , levels : List Level.Level
    , currentLevelId : Int
    }


initialLevels : List Level.Level
initialLevels =
    Level.allLevels


init : ( List Bool, Int ) -> ( Model, Cmd Msg )
init ( savedLevels, lastLevelId ) =
    let
        levels =
            List.map2 (\completed level -> { level | completed = completed }) savedLevels Level.allLevels

        levelId =
            lastLevelId
    in
    ( { currentScreen = Game
      , levels = levels
      , currentLevelId = 1
      , game = Game.init 1 levels
      }
    , Cmd.none
    )



---- PORTS ----


port saveLevels : List Bool -> Cmd msg



---- UPDATE ----


type Msg
    = GoToGame Level
    | ToggleLevelScreen
    | GotGameMsg Game.Msg
    | GotLevelsMsg Levels.Msg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotGameMsg subMsg ->
            let
                nextGame =
                    Game.update subMsg model.game

                nextLevels =
                    nextGame.levels
            in
            ( { model | game = nextGame, levels = nextLevels }
            , saveLevels (List.map (\l -> l.completed) nextLevels)
            )

        GotLevelsMsg _ ->
            ( model, Cmd.none )

        GoToGame level ->
            ( { model | currentScreen = Game, game = Game.init level.id model.levels }, Cmd.none )

        ToggleLevelScreen ->
            if model.currentScreen == Game then
                ( { model | currentScreen = Levels }, Cmd.none )

            else
                ( { model | currentScreen = Game }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        body =
            case model.currentScreen of
                Game ->
                    div [] [ Html.map (\gameMsg -> GotGameMsg gameMsg) (Game.view model.game) ]

                Levels ->
                    Levels.view goToGameCallback model.levels
    in
    div [ class "" ]
        [ div [ class "px-8 max-w-sm m-auto" ]
            [ header model
            , div [ class "border-b-2 border-gray-800 py-2" ] [ body ]
            ]
        ]


goToGameCallback : Level -> Html.Attribute Msg
goToGameCallback level =
    onClick (GoToGame level)


header : Model -> Html Msg
header model =
    let
        ( levelsButtonStyle, levelButtonText ) =
            case model.currentScreen of
                Game ->
                    ( "border-2 border-gray-800 text-gray-800 font-bold py-1 px-2 rounded", "Levels" )

                Levels ->
                    ( "border-2 border-gray-800 text-gray-800 font-bold py-1 px-2 rounded", "X" )
    in
    div [ class "flex w-full content-between border-b-2 border-gray-800 py-8" ]
        [ h1 [ class "flex-1 text-2xl font-bold text-gray-800 py-8 " ] [ text "Dot Jump" ]
        , div [ class "flex flex-1 justify-end items-center" ]
            [ button [ class levelsButtonStyle, onClick ToggleLevelScreen ] [ text levelButtonText ]
            ]
        ]



---- PROGRAM ----


type alias Flags =
    ( List Bool, Int )


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
