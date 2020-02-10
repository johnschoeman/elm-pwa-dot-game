module Model exposing
    ( Flags
    , Model(..)
    , Msg(..)
    , changeRouteTo
    , toSession
    )

import Browser
import Route
import Screen.Game.Main as Game
import Session
import Url


type alias Flags =
    ()


type Model
    = Loading Session.Model
    | Game Game.Model
    | NotFound Session.Model


type Msg
    = ChangedUrl Url.Url
    | ClickedLink Browser.UrlRequest
    | HandleGameMsg Game.Msg


toSession : Model -> Session.Model
toSession model =
    case model of
        NotFound v ->
            v

        Loading v ->
            v

        Game { session } ->
            session


changeRouteTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Game ->
            Game.init session
                |> updateWith Game HandleGameMsg


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )
