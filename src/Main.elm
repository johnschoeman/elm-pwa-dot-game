module Main exposing (..)

import Board exposing (..)
import Browser
import Dict
import Html exposing (Html, button, div, h1, img, text, ul)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)



---- MODEL ----


type alias Selection =
    Node


type alias Level =
    Int


type GameState
    = Won
    | Lost
    | InProgress


type alias Model =
    { board : Board
    , selection : Selection
    , level : Level
    , gameState : GameState
    }


init : ( Model, Cmd Msg )
init =
    ( { board = Board.level1, selection = A, level = 1, gameState = InProgress }, Cmd.none )



---- UPDATE ----


type Msg
    = SelectNode Node
    | ResetGame
    | IncrementLevel
    | DecrementLevel
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectNode toNode ->
            let
                fromNode =
                    model.selection

                maybeNeighborNode =
                    getNeighborNode fromNode toNode
            in
            case maybeNeighborNode of
                Just neighborNode ->
                    if isValid model.board fromNode neighborNode toNode then
                        let
                            nextBoard =
                                makeMove model.board fromNode neighborNode toNode

                            nextGameState =
                                gameState nextBoard
                        in
                        ( { model
                            | selection = toNode
                            , board = nextBoard
                            , gameState = nextGameState
                          }
                        , Cmd.none
                        )

                    else
                        ( { model | selection = toNode }, Cmd.none )

                Nothing ->
                    ( { model | selection = toNode }, Cmd.none )

        ResetGame ->
            let
                maybeLevel =
                    Dict.get model.level Board.levels
            in
            case maybeLevel of
                Just level ->
                    ( { model | board = level, gameState = InProgress }, Cmd.none )

                Nothing ->
                    ( { model | board = Board.level1, gameState = InProgress }, Cmd.none )

        IncrementLevel ->
            let
                nextLevelNumber =
                    model.level + 1

                maybeNextLevel =
                    Dict.get nextLevelNumber Board.levels
            in
            case maybeNextLevel of
                Just level ->
                    ( { model | level = nextLevelNumber, board = level, gameState = InProgress }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        DecrementLevel ->
            let
                nextLevelNumber =
                    model.level - 1

                maybeNextLevel =
                    Dict.get nextLevelNumber Board.levels
            in
            case maybeNextLevel of
                Just level ->
                    ( { model | level = nextLevelNumber, board = level, gameState = InProgress }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


gameState : Board -> GameState
gameState board =
    let
        count =
            dotCount board
    in
    if count == 1 then
        Won

    else if anyValidMoves board then
        InProgress

    else
        Lost


isValid : Board -> Node -> Node -> Node -> Bool
isValid board fromNode neighborNode destinationNode =
    hasDotAt board fromNode
        && not (hasDotAt board destinationNode)
        && hasDotAt board neighborNode


hasDotAt : Board -> Node -> Bool
hasDotAt board node =
    getDataAtNode isDot board node


isDot : Cell -> Bool
isDot cell =
    cell.status == Dot


makeMove : Board -> Node -> Node -> Node -> Board
makeMove board fromNode neighborNode toNode =
    setCell neighborNode Empty board
        |> setCell toNode Dot
        |> setCell fromNode Empty


setCell : Node -> Status -> Board -> Board
setCell node status board =
    updateBoardByNode node (setStatus status) board


setStatus : Status -> Cell -> Cell
setStatus status cell =
    { cell | status = status }



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        board =
            model.board

        selection =
            model.selection
    in
    div [ class "bg-gray-100" ]
        [ h1 [ class "text-gray-800 p-4 border-b-2 border-gray-800" ] [ text "Elm PWA Dot Game" ]
        , div [ class "p-8" ]
            [ header model
            , boardToHtml board selection
            ]
        ]


header : Model -> Html Msg
header model =
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
    in
    div [ class "flex flex-row justify-center items-center" ]
        [ text (String.fromInt (dotCount model.board))
        , text gameStateText
        , button
            [ onClick DecrementLevel, class buttonStyle ]
            [ text "<-" ]
        , div [ class "text-gray-800 px-4 py-4" ] [ text (String.fromInt model.level) ]
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
        [ div [ class rowStyle ] [ cellWithBoarder board.a, cellWithBoarder board.b, cellWithBoarder board.c ]
        , div [ class rowStyle ] [ cellWithBoarder board.d, cellWithBoarder board.e ]
        , div [ class rowStyle ] [ cellWithBoarder board.f, cellWithBoarder board.g, cellWithBoarder board.h ]
        , div [ class rowStyle ] [ cellWithBoarder board.i, cellWithBoarder board.j ]
        , div [ class rowStyle ] [ cellWithBoarder board.k, cellWithBoarder board.l, cellWithBoarder board.m ]
        ]


cellToHtml : Node -> Cell -> Html Msg
cellToHtml selection cell =
    let
        baseStyle =
            "flex items-center justify-center text-gray-700 text-center bg-gray-200 w-12 h-12 px-4 py-2 m-2"

        style =
            if cell.node == selection then
                baseStyle ++ " border-2 border-blue-600"

            else
                baseStyle

        content =
            case cell.status of
                Dot ->
                    " D "

                Empty ->
                    " - "
    in
    div [ class "flex justify-center items-center w-24" ]
        [ div [ class style, onClick (SelectNode cell.node) ] [ text content ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
