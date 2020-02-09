module Main exposing (..)

import Browser
import Dict exposing (..)
import Html exposing (Html, button, div, h1, img, text, ul)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)



---- MODEL ----


type Node
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M


type Status
    = Dot
    | Empty


type alias Start =
    String


type alias Neighbor =
    String


type alias Destination =
    String


type alias Cell =
    { node : Node
    , status : Status
    }


type alias Board =
    { a : Cell
    , b : Cell
    , c : Cell
    , d : Cell
    , e : Cell
    , f : Cell
    , g : Cell
    , h : Cell
    , i : Cell
    , j : Cell
    , k : Cell
    , l : Cell
    , m : Cell
    }


type alias Selection =
    Node


type alias Model =
    { board : Board
    , selection : Selection
    }



--- if a node has a neighbor, then the jump is considered possible by the game


getNeighborNode : Node -> Node -> Maybe Node
getNeighborNode fromNode toNode =
    case fromNode of
        A ->
            case toNode of
                C ->
                    Just B

                G ->
                    Just D

                K ->
                    Just F

                _ ->
                    Nothing

        B ->
            case toNode of
                F ->
                    Just D

                L ->
                    Just G

                H ->
                    Just E

                _ ->
                    Nothing

        C ->
            case toNode of
                A ->
                    Just B

                G ->
                    Just E

                M ->
                    Just H

                _ ->
                    Nothing

        D ->
            case toNode of
                J ->
                    Just G

                _ ->
                    Nothing

        E ->
            case toNode of
                I ->
                    Just G

                _ ->
                    Nothing

        F ->
            case toNode of
                B ->
                    Just D

                H ->
                    Just G

                L ->
                    Just I

                _ ->
                    Nothing

        G ->
            case toNode of
                A ->
                    Just D

                C ->
                    Just E

                K ->
                    Just I

                M ->
                    Just J

                _ ->
                    Nothing

        H ->
            case toNode of
                B ->
                    Just E

                F ->
                    Just G

                L ->
                    Just J

                _ ->
                    Nothing

        I ->
            case toNode of
                E ->
                    Just G

                _ ->
                    Nothing

        J ->
            case toNode of
                D ->
                    Just G

                _ ->
                    Nothing

        K ->
            case toNode of
                A ->
                    Just F

                G ->
                    Just I

                M ->
                    Just L

                _ ->
                    Nothing

        L ->
            case toNode of
                F ->
                    Just I

                B ->
                    Just G

                H ->
                    Just J

                _ ->
                    Nothing

        M ->
            case toNode of
                K ->
                    Just L

                G ->
                    Just J

                C ->
                    Just H

                _ ->
                    Nothing


a : Cell
a =
    { node = A
    , status = Dot
    }


b : Cell
b =
    { node = B
    , status = Dot
    }


c : Cell
c =
    { node = C
    , status = Empty
    }


d : Cell
d =
    { node = D
    , status = Dot
    }


e : Cell
e =
    { node = E
    , status = Empty
    }


f : Cell
f =
    { node = F
    , status = Empty
    }


g : Cell
g =
    { node = G
    , status = Empty
    }


h : Cell
h =
    { node = H
    , status = Empty
    }


i : Cell
i =
    { node = I
    , status = Dot
    }


j : Cell
j =
    { node = J
    , status = Empty
    }


k : Cell
k =
    { node = K
    , status = Dot
    }


l : Cell
l =
    { node = L
    , status = Empty
    }


m : Cell
m =
    { node = M
    , status = Empty
    }


initialBoard : Board
initialBoard =
    { a = a
    , b = b
    , c = c
    , d = d
    , e = e
    , f = f
    , g = g
    , h = h
    , i = i
    , j = j
    , k = k
    , l = l
    , m = m
    }


init : ( Model, Cmd Msg )
init =
    ( { board = initialBoard, selection = A }, Cmd.none )



---- UPDATE ----


type Msg
    = SelectNode Node
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
            [ text (nodeToString selection)
            , boardToHtml board selection
            ]
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
