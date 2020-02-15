module Screen.Game exposing (GameState(..), Model, Msg, init, update, updateLevel, view)

import Board exposing (Board, Node(..), Status(..), anyValidMoves, board1, boardDictionary, dotCount, getDataAtNode, getNeighborNode, moveIsValid, updateBoardByNode)
import Dict
import Html exposing (Html, a, button, div, h1, img, li, text, ul)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Level exposing (Level)



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
    , levelId = 1
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
        , boardToHtml model.board model.selection
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


boardToHtml : Board -> Node -> Html Msg
boardToHtml board selection =
    let
        cellWithBoarder =
            cellToHtml selection

        rowStyle =
            "flex flex-row justify-center items-center"
    in
    div [ class "flex-column p-4 items-center justify-center border-2" ]
        [ div [ class rowStyle ] [ cellWithBoarder A board.a, cellWithBoarder B board.b, cellWithBoarder C board.c ]
        , div [ class rowStyle ] [ cellWithBoarder D board.d, cellWithBoarder E board.e ]
        , div [ class rowStyle ] [ cellWithBoarder F board.f, cellWithBoarder G board.g, cellWithBoarder H board.h ]
        , div [ class rowStyle ] [ cellWithBoarder I board.i, cellWithBoarder J board.j ]
        , div [ class rowStyle ] [ cellWithBoarder K board.k, cellWithBoarder L board.l, cellWithBoarder M board.m ]
        ]


cellToHtml : Node -> Node -> Status -> Html Msg
cellToHtml selection cellNode cellStatus =
    let
        baseStyle =
            "flex items-center justify-center text-gray-700 text-center bg-gray-200 w-12 h-12 px-4 py-2 m-2"

        style =
            if cellNode == selection then
                baseStyle ++ " border-2 border-blue-600"

            else
                baseStyle

        content =
            case cellStatus of
                Dot ->
                    " D "

                BlackDot ->
                    " B "

                Empty ->
                    " - "
    in
    div [ class "flex justify-center items-center w-24" ]
        [ div [ class style, onClick (SelectNode cellNode) ] [ text content ]
        ]
