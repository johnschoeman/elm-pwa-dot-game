module Level exposing (Level, allLevels, level1, toString)

import Board exposing (Board, allBoards)


type alias Level =
    { id : Int, board : Board, completed : Bool }


level1 : Level
level1 =
    { id = 1
    , board = Board.board1
    , completed = False
    }


allLevels : List Level
allLevels =
    List.indexedMap (\idx board -> { id = idx, board = board, completed = False }) allBoards


toString : Level -> String
toString level =
    "Level " ++ String.fromInt level.id
