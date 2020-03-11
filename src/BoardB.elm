module BoardB exposing
    ( BoardB
    , NodeB(..)
    )

import NodeStatus exposing (NodeStatus(..))


type NodeB
    = A
    | B
    | C


type alias BoardB =
    { a : NodeStatus
    , b : NodeStatus
    , c : NodeStatus
    }


nodeBToString : NodeB -> String
nodeBToString node =
    case node of
        A ->
            "A"

        B ->
            "B"

        C ->
            "C"


getNeighborNode : NodeB -> NodeB -> Maybe Node
getNeighborNode fromNode toNode =
    case fromNode of
        A ->
            case toNode of
                C ->
                    Just B

                _ ->
                    Nothing

        B ->
            Nothing

        C ->
            case toNode of
                A ->
                    Just B

                _ ->
                    Nothing


updateBoardByNode : NodeB -> NodeStatus -> BoardB -> BoardB
