module View exposing (view)

import Browser
import Html exposing (..)
import Model exposing (Model(..), Msg(..))
import Screen.Game.Main as Game


view : Model -> Browser.Document Msg
view model =
    { title = "BlackDot"
    , body =
        [ h1 [] [ text "Black Dot" ]
        , pageContent model
        ]
    }


pageContent : Model -> Html Msg
pageContent model =
    case model of
        Game gameModel ->
            Html.map HandleGameMsg <| Game.view gameModel

        NotFound _ ->
            h2 [] [ text "Page not found" ]

        Loading _ ->
            text ""
