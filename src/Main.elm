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


type alias Model =
    { board : Board
    , selection : Selection
    , level : Level
    }


init : ( Model, Cmd Msg )
init =
    ( { board = Board.level1, selection = A, level = 1 }, Cmd.none )



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
                        ( { model
                            | selection = toNode
                            , board =
                                makeMove model.board fromNode neighborNode toNode
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
                    ( { model | board = level }, Cmd.none )

                Nothing ->
                    ( { model | board = Board.level1 }, Cmd.none )

        IncrementLevel ->
            let
                nextLevelNumber =
                    model.level + 1

                maybeNextLevel =
                    Dict.get nextLevelNumber Board.levels
            in
            case maybeNextLevel of
                Just level ->
                    ( { model | level = nextLevelNumber, board = level }, Cmd.none )

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
                    ( { model | level = nextLevelNumber, board = level }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


isValid : Board -> Node -> Node -> Node -> Bool
isValid board fromNode neighborNode destinationNode =
    hasDotAt board fromNode
        && not (hasDotAt board destinationNode)
        && hasDotAt board neighborNode


hasDotAt : Board -> Node -> Bool
hasDotAt board node =
    case node of
        A ->
            isDot board.a

        B ->
            isDot board.b

        C ->
            isDot board.c

        D ->
            isDot board.d

        E ->
            isDot board.e

        F ->
            isDot board.f

        G ->
            isDot board.g

        H ->
            isDot board.h

        I ->
            isDot board.i

        J ->
            isDot board.j

        K ->
            isDot board.k

        L ->
            isDot board.l

        M ->
            isDot board.m


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
    case node of
        A ->
            { board | a = setStatus board.a status }

        B ->
            { board | b = setStatus board.b status }

        C ->
            { board | c = setStatus board.c status }

        D ->
            { board | d = setStatus board.d status }

        E ->
            { board | e = setStatus board.e status }

        F ->
            { board | f = setStatus board.f status }

        G ->
            { board | g = setStatus board.g status }

        H ->
            { board | h = setStatus board.h status }

        I ->
            { board | i = setStatus board.i status }

        J ->
            { board | j = setStatus board.j status }

        K ->
            { board | k = setStatus board.k status }

        L ->
            { board | l = setStatus board.l status }

        M ->
            { board | m = setStatus board.m status }


setStatus : Cell -> Status -> Cell
setStatus cell status =
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
    in
    div [ class "flex flex-row justify-center items-center" ]
        [ button [ onClick DecrementLevel, class buttonStyle ] [ text "<-" ]
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


nodeToString : Node -> String
nodeToString node =
    case node of
        A ->
            "A"

        B ->
            "B"

        C ->
            "C"

        D ->
            "D"

        E ->
            "E"

        F ->
            "F"

        G ->
            "G"

        H ->
            "H"

        I ->
            "I"

        J ->
            "J"

        K ->
            "K"

        L ->
            "L"

        M ->
            "M"



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
