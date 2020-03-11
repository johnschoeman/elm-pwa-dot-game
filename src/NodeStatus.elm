module NodeStatus exposing (NodeStatus(..), show)


type NodeStatus
    = Dot
    | BlackDot
    | Empty


show : NodeStatus -> String
show status =
    case status of
        Empty ->
            "E"

        Dot ->
            "D"

        BlackDot ->
            "B"
