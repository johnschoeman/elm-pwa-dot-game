module Screen.Levels exposing (Model, Msg, update, view)

import Dict
import Html exposing (Html, a, button, div, h1, img, li, text, ul)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Level exposing (Level, allLevels, toString)



---- MODEL ----


type alias Model =
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


view : (Level -> Html.Attribute msg) -> Html msg
view navigateCallback =
    div []
        [ ul [] (List.map (levelButton navigateCallback) allLevels)
        ]


levelButton : (Level -> Html.Attribute msg) -> Level -> Html msg
levelButton navigateCallback level =
    li []
        [ button [ navigateCallback level ] [ text (Level.toString level) ]
        ]
