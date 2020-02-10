module Board exposing
    ( Board
    , Cell
    , Destination
    , Neighbor
    , Node(..)
    , Start
    , Status(..)
    , getNeighborNode
    , level1
    , levels
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
    { a =
        { node = A
        , status = Empty
        }
    , b =
        { node = B
        , status = Empty
        }
    , c =
        { node = C
        , status = Empty
        }
    , d =
        { node = D
        , status = Empty
        }
    , e =
        { node = E
        , status = Empty
        }
    , f =
        { node = F
        , status = Empty
        }
    , g =
        { node = G
        , status = Empty
        }
    , h =
        { node = H
        , status = Empty
        }
    , i =
        { node = I
        , status = Empty
        }
    , j =
        { node = J
        , status = Empty
        }
    , k =
        { node = K
        , status = Empty
        }
    , l =
        { node = L
        , status = Empty
        }
    , m =
        { node = M
        , status = Empty
        }
    }


createDot : Node -> Cell
createDot node =
    { node = node, status = Dot }


level1 : Board
level1 =
    { emptyBoard
        | g = createDot G
        , i = createDot I
        , j = createDot J
        , m = createDot M
    }


level2 : Board
level2 =
    { emptyBoard
        | a = createDot A
        , b = createDot B
        , d = createDot D
        , i = createDot I
        , k = createDot K
    }


level3 : Board
level3 =
    { emptyBoard
        | b = createDot B
        , g = createDot G
        , i = createDot I
        , l = createDot L
        , m = createDot M
    }


level4 : Board
level4 =
    { emptyBoard
        | f = createDot F
        , g = createDot G
        , i = createDot I
        , j = createDot J
        , k = createDot K
    }


level5 : Board
level5 =
    { emptyBoard
        | a = createDot A
        , e = createDot E
        , g = createDot G
        , h = createDot H
        , l = createDot L
    }


level6 : Board
level6 =
    { emptyBoard
        | b = createDot B
        , g = createDot G
        , i = createDot I
        , j = createDot J
        , l = createDot L
        , m = createDot M
    }


level7 : Board
level7 =
    { emptyBoard
        | e = createDot E
        , f = createDot F
        , g = createDot G
        , i = createDot I
        , j = createDot J
        , k = createDot K
    }


level8 : Board
level8 =
    { emptyBoard
        | a = createDot A
        , b = createDot B
        , f = createDot F
        , i = createDot I
        , k = createDot K
        , l = createDot L
    }


level9 : Board
level9 =
    { emptyBoard
        | b = createDot B
        , d = createDot D
        , e = createDot E
        , g = createDot G
        , i = createDot I
        , j = createDot J
        , l = createDot L
    }


level10 : Board
level10 =
    { emptyBoard
        | b = createDot B
        , d = createDot D
        , e = createDot E
        , i = createDot I
        , j = createDot J
        , l = createDot L
        , m = createDot M
    }