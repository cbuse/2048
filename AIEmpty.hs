module AIEmpty where

import Board
import Interactive


--Se obtine o lista de perechi cu board-ul modificat dupa fiecare mutare si numarul de celule libere
all_moves :: Board -> [(Board, Int)]
all_moves brd = [(movedBoardLeft, length (zeroIndexes movedBoardLeft)),
				(movedBoardRight, length (zeroIndexes movedBoardRight)),
				(movedBoardUp, length (zeroIndexes movedBoardUp)),
				(movedBoardDown, length (zeroIndexes movedBoardDown))]
	where
	movedBoardLeft = (moveLeft brd)
	movedBoardRight = (moveRight brd)
	movedBoardUp = (moveUp brd)
	movedBoardDown = (moveDown brd)
	
{-

    Întoarce tabla rezultată din aplicarea acelei mutări care maximizează
    numărul de celule libere.
-}
move :: Board -> Board
move brd = fst (head(filter (\(a,b) -> (b == maximum (map snd (all_moves brd)))) (all_moves brd)))


{-
    Urmărește pas cu pas evoluția jocului, conform strategiei implementate.
-}
userMode :: IO ()
userMode = ai move
