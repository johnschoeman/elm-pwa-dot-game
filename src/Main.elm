module Main exposing (..)

import Model
import Update
import View


main : Program Model.Flags Model.Model Model.Msg
main =
    Browser.application
        { view = View.view
        , init = Update.init
        , update = Update.update
        , subscriptions = Update.subscriptions
        , onUrlChange = Model.ChangedUrl
        , onUrlRequest = Model.ClickedLink
        }
