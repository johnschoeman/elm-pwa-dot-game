module Board exposing
    ( Board
    , Destination
    , Neighbor
    , Node(..)
    , Start
    , Status(..)
    , anyValidMoves
    , dotCount
    , getDataAtNode
    , getNeighborNode
    , level1
    , levels
    , moveIsValid
    , updateBoardByNode
    )

import Dict


type alias A =
    Node


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


allNodes : List Node
allNodes =
    [ A, B, C, D, E, F, G, H, I, J, K, L, M ]


type Status
    = Dot
    | BlackDot
    | Empty


type alias Start =
    String


type alias Neighbor =
    String


type alias Destination =
    String


type alias Board =
    { a : Status
    , b : Status
    , c : Status
    , d : Status
    , e : Status
    , f : Status
    , g : Status
    , h : Status
    , i : Status
    , j : Status
    , k : Status
    , l : Status
    , m : Status
    }


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


statusToString : Status -> String
statusToString status =
    case status of
        Empty ->
            "E"

        Dot ->
            "D"

        BlackDot ->
            "B"



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


updateBoardByNode : Node -> Status -> Board -> Board
updateBoardByNode node status board =
    case node of
        A ->
            { board | a = status }

        B ->
            { board | b = status }

        C ->
            { board | c = status }

        D ->
            { board | d = status }

        E ->
            { board | e = status }

        F ->
            { board | f = status }

        G ->
            { board | g = status }

        H ->
            { board | h = status }

        I ->
            { board | i = status }

        J ->
            { board | j = status }

        K ->
            { board | k = status }

        L ->
            { board | l = status }

        M ->
            { board | m = status }


getDataAtNode : Board -> Node -> Status
getDataAtNode board node =
    case node of
        A ->
            board.a

        B ->
            board.b

        C ->
            board.c

        D ->
            board.d

        E ->
            board.e

        F ->
            board.f

        G ->
            board.g

        H ->
            board.h

        I ->
            board.i

        J ->
            board.j

        K ->
            board.k

        L ->
            board.l

        M ->
            board.m


dotCount : Board -> Int
dotCount board =
    List.foldl (addDots board) 0 allNodes


addDots : Board -> Node -> Int -> Int
addDots board node acc =
    hasDot (getDataAtNode board node) + acc


hasDot : Status -> Int
hasDot status =
    if status == Dot then
        1

    else
        0


anyValidMoves : Board -> Bool
anyValidMoves board =
    List.foldr (anyValidMovesOnBoard board) False allNodes


anyValidMovesOnBoard : Board -> Node -> Bool -> Bool
anyValidMovesOnBoard board fromNode acc =
    case acc of
        True ->
            True

        False ->
            hasValidMove board fromNode


hasValidMove : Board -> Node -> Bool
hasValidMove board fromNode =
    List.foldl (anyValidMovesForNode board fromNode) False allNodes


anyValidMovesForNode : Board -> Node -> Node -> Bool -> Bool
anyValidMovesForNode board fromNode toNode acc =
    case acc of
        True ->
            True

        False ->
            moveIsValid board fromNode toNode


moveIsValid : Board -> Node -> Node -> Bool
moveIsValid board fromNode toNode =
    let
        maybeNeighborNode =
            getNeighborNode fromNode toNode
    in
    case maybeNeighborNode of
        Nothing ->
            False

        Just neighborNode ->
            let
                fromStatus =
                    getDataAtNode board fromNode

                neighborStatus =
                    getDataAtNode board neighborNode

                toStatus =
                    getDataAtNode board toNode
            in
            (fromStatus == Dot || fromStatus == BlackDot)
                && (neighborStatus == Dot && neighborStatus /= BlackDot)
                && (toStatus == Empty)


levels : Dict.Dict Int Board
levels =
    Dict.fromList
        [ ( 1, level1 )
        , ( 2, level2 )
        , ( 3, level3 )
        , ( 4, level4 )
        , ( 5, level5 )
        , ( 6, level6 )
        , ( 7, level7 )
        , ( 8, level8 )
        , ( 9, level9 )
        , ( 10, level10 )
        ]


emptyBoard : Board
emptyBoard =
    { a = Empty
    , b = Empty
    , c = Empty
    , d = Empty
    , e = Empty
    , f = Empty
    , g = Empty
    , h = Empty
    , i = Empty
    , j = Empty
    , k = Empty
    , l = Empty
    , m = Empty
    }


level1 : Board
level1 =
    { emptyBoard
        | g = Dot
        , i = BlackDot
        , j = Dot
        , m = Dot
    }


level2 : Board
level2 =
    { emptyBoard
        | a = Dot
        , b = BlackDot
        , d = Dot
        , i = Dot
        , k = Dot
    }


level3 : Board
level3 =
    { emptyBoard
        | b = Dot
        , g = Dot
        , i = Dot
        , l = Dot
        , m = Dot
    }


level4 : Board
level4 =
    { emptyBoard
        | f = Dot
        , g = Dot
        , i = Dot
        , j = Dot
        , k = Dot
    }


level5 : Board
level5 =
    { emptyBoard
        | a = Dot
        , e = Dot
        , g = Dot
        , h = Dot
        , l = Dot
    }


level6 : Board
level6 =
    { emptyBoard
        | b = Dot
        , g = Dot
        , i = Dot
        , j = Dot
        , l = Dot
        , m = Dot
    }


level7 : Board
level7 =
    { emptyBoard
        | e = Dot
        , f = Dot
        , g = Dot
        , i = Dot
        , j = Dot
        , k = Dot
    }


level8 : Board
level8 =
    { emptyBoard
        | a = Dot
        , b = Dot
        , f = Dot
        , i = Dot
        , k = Dot
        , l = Dot
    }


level9 : Board
level9 =
    { emptyBoard
        | b = Dot
        , d = Dot
        , e = Dot
        , g = Dot
        , i = Dot
        , j = Dot
        , l = Dot
    }


level10 : Board
level10 =
    { emptyBoard
        | b = Dot
        , d = Dot
        , e = Dot
        , i = Dot
        , j = Dot
        , l = Dot
        , m = Dot
    }
