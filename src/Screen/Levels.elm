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


view : (Level -> Html.Attribute msg) -> List Level -> Html msg
view navigateCallback levels =
    div []
        [ ul [] (List.map (levelListItem navigateCallback) levels)
        ]


levelListItem : (Level -> Html.Attribute msg) -> Level -> Html msg
levelListItem navigateCallback level =
    li []
        [ button [ navigateCallback level ] [ text (Level.toString level) ]
        , levelCompletedText level
        ]


levelCompletedText : Level -> Html msg
levelCompletedText level =
    if level.completed then
        text "C"

    else
        text "-"
