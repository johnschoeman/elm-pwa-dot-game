module Screen.Game exposing (GameState(..), Model, Msg, init, update, updateLevel, view)

import Board exposing (Board, Node(..), Status(..), anyValidMoves, board1, boardDictionary, dotCount, getDataAtNode, getNeighborNode, moveIsValid, updateBoardByNode)
import Dict
import Html exposing (Html, a, button, div, h1, img, li, text, ul)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Level exposing (Level)
import Svg exposing (Svg, circle, line, svg)
import Svg.Attributes exposing (color, cx, cy, fill, r, stroke, strokeWidth, viewBox, x1, x2, y1, y2)
import Svg.Events



---- MODEL ----


type alias Selection =
    Node


type GameState
    = Won
    | Lost
    | InProgress


type alias Model =
    { board : Board
    , selection : Selection
    , levelId : Int
    , gameState : GameState
    }


init : Model
init =
    { board = Board.board1
    , selection = A
    , levelId = 0
    , gameState = InProgress
    }



---- UPDATE ----


type Msg
    = SelectNode Node
    | ResetGame
    | IncrementLevel
    | DecrementLevel
    | NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectNode toNode ->
            let
                fromNode =
                    model.selection

                nextBoard =
                    makeMove model.board fromNode toNode

                nextGameState =
                    gameState nextBoard
            in
            { model
                | selection = toNode
                , board = nextBoard
                , gameState = nextGameState
            }

        ResetGame ->
            resetGame model

        IncrementLevel ->
            let
                nextLevelNumber =
                    model.levelId + 1

                maybeNextLevel =
                    Dict.get nextLevelNumber Board.boardDictionary
            in
            case maybeNextLevel of
                Just level ->
                    { model | levelId = nextLevelNumber, board = level, gameState = InProgress }

                Nothing ->
                    model

        DecrementLevel ->
            let
                nextLevelNumber =
                    model.levelId - 1

                maybeNextLevel =
                    Dict.get nextLevelNumber Board.boardDictionary
            in
            case maybeNextLevel of
                Just level ->
                    { model | levelId = nextLevelNumber, board = level, gameState = InProgress }

                Nothing ->
                    model

        NoOp ->
            model


updateLevel : Level -> Model -> Model
updateLevel level model =
    setLevel level model
        |> resetGame


setLevel : Level -> Model -> Model
setLevel level model =
    { model | levelId = level.id }


resetGame : Model -> Model
resetGame model =
    let
        maybeLevel =
            Dict.get model.levelId Board.boardDictionary
    in
    case maybeLevel of
        Just level ->
            { model | board = level, gameState = InProgress }

        Nothing ->
            { model | board = Board.board1, gameState = InProgress }


gameState : Board -> GameState
gameState board =
    let
        count =
            dotCount board
    in
    if count == 0 then
        Won

    else if anyValidMoves board then
        InProgress

    else
        Lost


makeMove : Board -> Node -> Node -> Board
makeMove board fromNode toNode =
    let
        fromStatus =
            getDataAtNode board fromNode

        maybeNeighborNode =
            getNeighborNode fromNode toNode
    in
    case maybeNeighborNode of
        Just neighborNode ->
            if moveIsValid board fromNode toNode then
                case fromStatus of
                    BlackDot ->
                        setCell neighborNode Empty board
                            |> setCell toNode BlackDot
                            |> setCell fromNode Empty

                    Dot ->
                        setCell neighborNode Empty board
                            |> setCell toNode Dot
                            |> setCell fromNode Empty

                    Empty ->
                        board

            else
                board

        Nothing ->
            board


setCell : Node -> Status -> Board -> Board
setCell node status board =
    updateBoardByNode node status board



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ gameHeader model
        , boardToSvg model.board model.selection
        ]


gameHeader : Model -> Html Msg
gameHeader model =
    let
        buttonStyle =
            "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"

        gameStateText =
            case model.gameState of
                Won ->
                    "YOU WON"

                Lost ->
                    "YOU LOST"

                InProgress ->
                    ""

        currentLevelText =
            "Level " ++ String.fromInt model.levelId
    in
    div [ class "flex flex-row justify-center items-center" ]
        [ text (String.fromInt (dotCount model.board))
        , text gameStateText
        , button
            [ onClick DecrementLevel, class buttonStyle ]
            [ text "<-" ]
        , div [ class "text-gray-800 px-4 py-4" ] [ text currentLevelText ]
        , button [ onClick IncrementLevel, class buttonStyle ] [ text "->" ]
        , button [ onClick ResetGame, class (buttonStyle ++ " ml-8") ] [ text "Reset" ]
        ]


boardToSvg : Board -> Node -> Html Msg
boardToSvg board selection =
    let
        cellWithSelection =
            cellToSvg selection

        scaleFactor =
            40

        row1 =
            String.fromInt (scaleFactor * 1)

        row2 =
            String.fromInt (scaleFactor * 2)

        row3 =
            String.fromInt (scaleFactor * 3)

        row4 =
            String.fromInt (scaleFactor * 4)

        row5 =
            String.fromInt (scaleFactor * 5)

        col1 =
            String.fromInt (scaleFactor * 1)

        col2 =
            String.fromInt (scaleFactor * 2)

        col3 =
            String.fromInt (scaleFactor * 3)

        col4 =
            String.fromInt (scaleFactor * 4)

        col5 =
            String.fromInt (scaleFactor * 5)
    in
    svg [ viewBox "0 0 300 300", strokeWidth "3", stroke "black" ]
        [ -- long diagonals
          line [ x1 row1, x2 row5, y1 col1, y2 col5, stroke "black" ] []
        , line [ x1 row5, x2 row1, y1 col1, y2 col5, stroke "black" ] []

        -- short diagonals
        , line [ x1 row1, x2 row3, y1 col3, y2 col5, stroke "black" ] []
        , line [ x1 row3, x2 row1, y1 col1, y2 col3, stroke "black" ] []
        , line [ x1 row3, x2 row5, y1 col1, y2 col3, stroke "black" ] []
        , line [ x1 row3, x2 row5, y1 col5, y2 col3, stroke "black" ] []

        -- horizontals
        , line [ x1 row1, x2 row1, y1 col1, y2 col5, stroke "black" ] []
        , line [ x1 row3, x2 row3, y1 col1, y2 col5, stroke "black" ] []
        , line [ x1 row5, x2 row5, y1 col1, y2 col5, stroke "black" ] []
        , line [ x1 row1, x2 row5, y1 col1, y2 col1, stroke "black" ] []
        , line [ x1 row1, x2 row5, y1 col3, y2 col3, stroke "black" ] []
        , line [ x1 row1, x2 row5, y1 col5, y2 col5, stroke "black" ] []
        , cellWithSelection A board.a col1 row1
        , cellWithSelection B board.b col3 row1
        , cellWithSelection C board.c col5 row1
        , cellWithSelection D board.d col2 row2
        , cellWithSelection E board.e col4 row2
        , cellWithSelection F board.f col1 row3
        , cellWithSelection G board.g col3 row3
        , cellWithSelection H board.h col5 row3
        , cellWithSelection I board.i col2 row4
        , cellWithSelection J board.j col4 row4
        , cellWithSelection K board.k col1 row5
        , cellWithSelection L board.l col3 row5
        , cellWithSelection M board.m col5 row5
        ]


cellToSvg : Node -> Node -> Status -> String -> String -> Html Msg
cellToSvg selection cellNode cellStatus xPos yPos =
    let
        circleFill =
            case cellStatus of
                Dot ->
                    fill "blue"

                BlackDot ->
                    fill "black"

                Empty ->
                    fill "white"

        circleStroke =
            if selection == cellNode then
                stroke "green"

            else
                stroke "black"
    in
    svg
        [ Svg.Events.onClick (SelectNode cellNode)
        , circleFill
        , circleStroke
        ]
        [ circle [ cx xPos, cy yPos, r "15" ] []
        ]
