{-# OPTIONS_GHC -F -pgmF htfpp #-}

module AIEmptyTest where

import Board
import AIEmpty
import Interactive
import System.Random
import Test.Framework

merge :: [[Int]]
merge = [ [ 2, 2, 0, 0 ]
        , [ 2, 2, 2, 0 ]
        , [ 2, 2, 2, 2 ]
        , [ 2, 2, 4, 4 ]
        ]

test_move :: IO ()
test_move = assertBool
    $ (rows $ move $ build merge 0) `elem` [ [ [ 4, 0, 0, 0 ]
                                             , [ 4, 2, 0, 0 ]
                                             , [ 4, 4, 0, 0 ]
                                             , [ 4, 8, 0, 0 ]
                                             ]
                                           , [ [ 0, 0, 0, 4 ]
                                             , [ 0, 0, 2, 4 ]
                                             , [ 0, 0, 4, 4 ]
                                             , [ 0, 0, 4, 8 ]
                                             ]
                                           ]

test_play :: IO ()
test_play = do
    initialBoard <- getStdRandom initialize
    finalBoard   <- getStdRandom $ play move initialBoard
    print finalBoard
    assertBool $ score finalBoard > 0
