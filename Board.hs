{-
    Tabla de joc și mutările posibile.

    Modulul exportă numai funcțiile enumerate mai jos, ceea ce înseamnă
    că doar acestea vor fi vizibile din alte module. Justificarea vizează
    prevenirea accesului extern la structura obiectelor 'Board', și a eventualei
    coruperi a consistenței interne a acestora.
-}
module Board
    ( Board
    , build
    , rows
    , score
    , initialize
    , placeRandomCell
    , moveUp
    , moveDown
    , moveLeft
    , moveRight
    , isWon
    , isLost
	, zeroIndexes
    ) where

import System.Random
--import Control.Monad.Random
--IMPORTURI ADAUGATE DE MINE NU SE STIE POATE NU E NEVOIE DE TOATE
--import Control.Monad.Random
--import Control.Monad.Writer
--import Data.Monoid
import Data.Maybe
import Data.List

import qualified Data.Text as T


--Tabla de joc este definita printr-o lista de liste cu elemente de tip Int(o voi numi matrice de acum incolo) si un scor de tip Int
data Board = Board
  { lines :: [[Int]]
  , score_sum :: Int
  } deriving (Eq)


{-

    Instanțiați clasa 'Show' cu tipul 'Board'. Exemplu de reprezentare:

       . |    4 |    2 |    4
       2 |    . |    4 |   32
       . |    . |    8 |  128
       8 |   32 |   64 |    8
    Score: 1216
-}

--Reprezentarea nu este ca cea din exemplu, in sensul ca in loc de . am afisat 0,
--iar atunci cand numerele sunt mai lungi de o cifra se afiseaza decalat |
--Exemplu de afisare
{-0 | 4 | 4 | 4
2 | 0 | 4 | 2
2 | 2 | 0 | 0
0 | 2 | 0 | 2
Score 2048
-}

instance Show Board where
    show (Board cells sc) = (unlines $ map (intercalate " | " . map show) cells) ++ "Score: " ++ (show sc)


{-
   
    Construiește o tablă de joc pe baza unei configurații, furnizate pe linii,
    și a unui scor.
-}


build :: [[Int]] -> Int -> Board
build list game_score = Board list game_score

{-
   

    Întoarce configurația tablei de joc.
-}

--am folosit pattern matching
rows :: Board -> [[Int]]
rows (Board cells game_score) = cells


{-
    

    Întoarce scorul curent al tablei de joc.
-}

--am folosit pattern matching
score :: Board -> Int
score (Board cells game_score) = game_score

{-
   

    Plasează aleator o nouă celulă pe tabla de joc.

    Aveți grijă să nu modificați celulele existente!
-}


--se alege aleator o celula libera(cu valoarea 0) si se plaseaza in ea 2 sau 4
placeRandomCell :: RandomGen g => Board -> g -> (Board, g)
placeRandomCell brd g = ((update brd index newcell), ng)
	where
	index = randIndex (snd(next g)) (zeroIndexes brd)
	newcell = randNewCell (snd(next g)) probabilities
	(a, ng) = next g
	
--lista care contine pe pozitia 0 valoarea 4 si pe celelalte 9 pozitii valoarea 2
--am procedat astfel pentru a obtine aceeasi probabilitate pentru aparitiile lui 2 sau 4
--ca in jocul original	
probabilities = 2 : take 9  (repeat 2)

--alege 4 cu probabilitatea de 0.1 si 2 cu probabilitatea de 0.9
randNewCell :: RandomGen b => b -> [a] -> a	
randNewCell g probabilities = probabilities !! (fst (randomR (0, no) g)) 
	where no = (length probabilities) - 1

--returneaza o lista cu indecsii elementelor 0 dintr-un board
zeroIndexes :: Board -> [Int]
zeroIndexes (Board b_lines sc) = elemIndices 0 (concat b_lines) 

--alege aleator un index din lista de indecsi ai elementelor 0 ai board-ului pe baza unui generator	
randIndex :: RandomGen b => b -> [a] -> a
randIndex g listIndexes = listIndexes !! (fst (randomR (0, no) g)) 
	where no = (length listIndexes) - 1

--inlocuieste elementul de pe pozitia index din board cu alt element new
replace :: Board -> Int -> Int -> [Int]
replace (Board cells sc) index new = (take index list16) ++ [new] ++  (drop (index + 1) list16)
	where list16 = concat cells

--transforma o lista de 16 elemente intr-o matrice
listToLists :: [a] -> [[a]]
listToLists l = (take 4 l) : (take 4 (drop 4 l)) : (take 4 (drop 8 l)) : (take 4 (drop 12 l)) : []

--updateaza boardul: valoarea de la un index dat se modifica
update :: Board -> Int -> Int ->  Board
update brd index new  = build  (listToLists (replace brd index new)) (score brd)

{-
    

    Generează aleator o tablă de joc cu două celule ocupate.
-}


--Se aplica placeRandomCell de 2 ori pe un board gol(care are doar elemente 0)
initialize :: RandomGen g => g -> (Board, g)
initialize g = ( fst ( placeRandomCell random1 g) , snd(next g))
	where random1 = fst(placeRandomCell emptyBoard (snd (next g)))

emptyBoard = build (take 4 $ repeat $ take 4 $ repeat 0) 0

{-
    
    Cele patru mutări posibile: stânga, dreapta, sus, jos.
    
    Acestea sunt singurele funcții care modifică scorul.
-}

--Calculez scorul pentru o linie, explorand toate posibilitatile
collapse :: (Eq a, Num a) => a -> [a] -> (a, [a])
collapse acc [] = (acc, [])

collapse acc [x] = (acc, [x])

collapse acc [x,y] = if x == y then  ((acc + x + y), [ x + y]) else (acc, [x,y])

collapse acc [x,y,z]  
	|x == y =  (acc + x + y  , [x + y, z])
	|y == z =  (acc + y + z  , [x, y + z])
	|otherwise = (acc , [x,y,z])
	
collapse acc [x,y,z,t]  
	|(x == y && z == t) =  (acc + x + y + z + t  , [x + y, z + t])
	|x == y =  (acc + x + y  , [x + y, z, t])
	|y == z =  (acc + y + z  , [x , y + z, t])
	|z == t =  (acc + t + z  , [x , y , z + t])
	|otherwise = (acc , [x,y,z,t])	
	
--Se obtine o linie trasa spre stanga(completata cu 0 daca e cazul), precum si scorul ei
combinedLine :: (Eq a, Num a) => [a] -> (a, [a])
combinedLine line = ((fst useful) ,(take 4  ((snd useful) ++ repeat 0)))
	where useful = (collapse 0 (filter ( /= 0) line))

--Se obtine surplusul de scor generat de matricea cells, precum si matricea modificata 
--in urma unei mutari la stanga
my_move :: (Eq a, Num a) => [[a]] -> (a, [[a]])
my_move cells = (scor, (map snd useful))
	where
	useful = map combinedLine cells
	scor = foldl (+) 0 (map fst useful)
	
--Se obtine un board cu noul scor si modificarile aferente unei mutari la stanga
moveLeft :: Board -> Board
moveLeft brd = build (snd useful) (fst useful + (score brd))
	where useful = my_move (rows brd)

--Se obtine un board cu noul scor si modificarile aferente unei mutari sus	
moveUp :: Board -> Board
moveUp (Board cells game_score) = build useful (scor + game_score)
	where
	useful = transpose (snd (my_move (transpose cells)))
	scor = fst (my_move (transpose cells))
	
--Se obtine un board cu noul scor si modificarile aferente unei mutari la dreapta
moveRight :: Board -> Board
moveRight (Board cells game_score) = build useful (scor + game_score)
	where
	useful = map reverse (snd (my_move (map reverse cells)))
	scor = fst (my_move (map reverse cells))

--Se obtine un board cu noul scor si modificarile aferente unei mutari jos
moveDown :: Board -> Board
moveDown (Board cells game_score) = build useful (scor + game_score)
	where
	useful = transpose (map reverse (snd (my_move (map reverse (transpose cells)))))
	scor = fst (my_move (map reverse (transpose cells)))

{-
   

    Întoarce 'True' dacă tabla conține o configurație câștigătoare,
    i.e. există cel puțin o celulă cu 2048.
-}
isWon :: Board -> Bool
isWon (Board cells scorr) = 2048 `elem` (concat cells)

{-
  

    Întoarce 'True' dacă tabla conține o configurație în care jucătorul pierde,
    i.e. nu există nicio celulă liberă, și nici nu există celule vecine egale,
    pe orizontală sau verticală.
-}

isLost :: Board -> Bool
isLost (Board cells scorr) = (not ( 0 `elem` (concat cells))) &&
							 (noMoreHorizontalMoves (Board cells scorr) ) 
							 && (noMoreVerticalMoves (Board cells scorr))

--verifica daca o linie contine 2 elemente alaturate egale
--verificarea se face comparand daca elementul de pe pozitia i este diferit de cel de pe pozitia i + 1
--True = nu mai pot sa mut nimic pe acea linie
noEqualNeighbors :: Eq b => [b] -> Bool
noEqualNeighbors xs = and . zipWith (/=) xs $ tail xs

--verifica daca se mai poate face vreo mutare pe orizontala
--True = nu se mai pot face mutari orizontale
noMoreHorizontalMoves :: Board -> Bool
noMoreHorizontalMoves (Board cells game_score) = not (False `elem` (map noEqualNeighbors cells))

--verifica daca se mai poate face vreo mutare pe verticală
--True = nu se mai pot face mutari verticala
noMoreVerticalMoves :: Board -> Bool
noMoreVerticalMoves (Board cells game_score) = not (False `elem` (map noEqualNeighbors (transpose cells)))
