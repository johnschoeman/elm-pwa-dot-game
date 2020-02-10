module Update exposing
    ( init
    , subscriptions
    , update
    )

import Browser
import Browser.Navigation as Nav
import Model exposing (..)
import Route
import Screen.Game.Main as Game
import Session
import Url


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        session =
            Session.initial key
    in
    changeRouteTo (Route.fromUrl url)
        (Loading session)



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( HandleGameMsg subMsg, Game model_ ) ->
            let
                ( newModel, newMsg ) =
                    Game.update model_ subMsg
            in
            ( Game newModel
            , Cmd.map HandleGameMsg newMsg
            )

        ( _, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Game homeModel ->
            Sub.map HandleGameMsg <| Game.subscriptions homeModel

        _ ->
            Sub.none
