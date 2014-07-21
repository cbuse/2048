{-# OPTIONS_GHC -F -pgmF htfpp #-}

module BoardTest where

import Board
import System.Random
import Control.Monad
import Test.Framework

single :: [[Int]]
single = [ [ 2, 0, 0, 0 ]
         , [ 0, 2, 0, 0 ]
         , [ 0, 0, 2, 0 ]
         , [ 0, 0, 0, 2 ]
         ]

multiple :: [[Int]]
multiple = [ [ 2, 4, 0, 0 ]
           , [ 4, 2, 8, 0 ]
           , [ 0, 8, 2, 4 ]
           , [ 0, 0, 4, 2 ]
           ]

merge :: [[Int]]
merge = [ [ 2, 2, 0, 0 ]
        , [ 2, 2, 2, 0 ]
        , [ 2, 2, 2, 2 ]
        , [ 2, 2, 4, 4 ]
        ]

test_build :: IO ()
test_build = do
    assertEqual single $ rows board
    assertEqual 2048 $ score board
  where
    board = build single 2048

test_initialize :: IO ()
test_initialize = do
    board <- getStdRandom initialize
    let cells = filter (/= 0) $ concat $ rows board
    -- Exact 2 celule
    assertEqual 2 $ length cells
    -- Ambele 2 sau 4
    assertBool $ and $ map (`elem` [2, 4]) cells
    -- Scor inițial 0
    assertEqual 0 $ score board

test_placeRandomCell :: IO ()
test_placeRandomCell = do
    board <- getStdRandom $ placeRandomCell $ build multiple 0
    let cells1 = concat multiple
    let cells2 = concat $ rows board
    -- O singură celulă diferită după generare
    assertEqual 1
        $ length $ filter (liftM2 (/=) fst snd) $ zip cells1 cells2

test_moveLeft :: IO ()
test_moveLeft = do
    -- single
    let board = moveLeft $ build single 0 in do
        assertEqual singleMoved $ rows board
        assertEqual 0 $ score board
    -- multiple
    let board = moveLeft $ build multiple 0 in do
        assertEqual multipleMoved $ rows board
        assertEqual 0 $ score board
    -- merge
    let board = moveLeft $ build merge 0 in do
        assertEqual mergeMoved $ rows board
        assertEqual 28 $ score board
  where
    singleMoved   = [ [ 2, 0, 0, 0 ]
                    , [ 2, 0, 0, 0 ]
                    , [ 2, 0, 0, 0 ]
                    , [ 2, 0, 0, 0 ]
                    ]
    multipleMoved = [ [ 2, 4, 0, 0 ]
                    , [ 4, 2, 8, 0 ]
                    , [ 8, 2, 4, 0 ]
                    , [ 4, 2, 0, 0 ]
                    ]
    mergeMoved    = [ [ 4, 0, 0, 0 ]
                    , [ 4, 2, 0, 0 ]
                    , [ 4, 4, 0, 0 ]
                    , [ 4, 8, 0, 0 ]
                    ]

test_moveRight :: IO ()
test_moveRight = do
    -- single
    let board = moveRight $ build single 0 in do
        assertEqual singleMoved $ rows board
        assertEqual 0 $ score board
    -- multiple
    let board = moveRight $ build multiple 0 in do
        assertEqual multipleMoved $ rows board
        assertEqual 0 $ score board
    -- merge
    let board = moveRight $ build merge 0 in do
        assertEqual mergeMoved $ rows board
        assertEqual 28 $ score board
  where
    singleMoved   = [ [ 0, 0, 0, 2 ]
                    , [ 0, 0, 0, 2 ]
                    , [ 0, 0, 0, 2 ]
                    , [ 0, 0, 0, 2 ]
                    ]
    multipleMoved = [ [ 0, 0, 2, 4 ]
                    , [ 0, 4, 2, 8 ]
                    , [ 0, 8, 2, 4 ]
                    , [ 0, 0, 4, 2 ]
                    ]
    mergeMoved    = [ [ 0, 0, 0, 4 ]
                    , [ 0, 0, 2, 4 ]
                    , [ 0, 0, 4, 4 ]
                    , [ 0, 0, 4, 8 ]
                    ]

test_moveUp :: IO ()
test_moveUp = do
    -- single
    let board = moveUp $ build single 0 in do
        assertEqual singleMoved $ rows board
        assertEqual 0 $ score board
    -- multiple
    let board = moveUp $ build multiple 0 in do
        assertEqual multipleMoved $ rows board
        assertEqual 0 $ score board
    -- merge
    let board = moveUp $ build merge 0 in do
        assertEqual mergeMoved $ rows board
        assertEqual 20 $ score board
  where
    singleMoved   = [ [ 2, 2, 2, 2 ]
                    , [ 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0 ]
                    ]
    multipleMoved = [ [ 2, 4, 8, 4 ]
                    , [ 4, 2, 2, 2 ]
                    , [ 0, 8, 4, 0 ]
                    , [ 0, 0, 0, 0 ]
                    ]
    mergeMoved    = [ [ 4, 4, 4, 2 ]
                    , [ 4, 4, 4, 4 ]
                    , [ 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0 ]
                    ]

test_moveDown :: IO ()
test_moveDown = do
    -- single
    let board = moveDown $ build single 0 in do
        assertEqual singleMoved $ rows board
        assertEqual 0 $ score board
    -- multiple
    let board = moveDown $ build multiple 0 in do
        assertEqual multipleMoved $ rows board
        assertEqual 0 $ score board
    -- merge
    let board = moveDown $ build merge 0 in do
        assertEqual mergeMoved $ rows board
        assertEqual 20 $ score board
  where
    singleMoved   = [ [ 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0 ]
                    , [ 2, 2, 2, 2 ]
                    ]
    multipleMoved = [ [ 0, 0, 0, 0 ]
                    , [ 0, 4, 8, 0 ]
                    , [ 2, 2, 2, 4 ]
                    , [ 4, 8, 4, 2 ]
                    ]
    mergeMoved    = [ [ 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0 ]
                    , [ 4, 4, 4, 2 ]
                    , [ 4, 4, 4, 4 ]
                    ]

test_isWon :: IO ()
test_isWon = do
    assertBool $ isWon $ build [ [ 2, 0, 0, 2048 ]
                               , [ 0, 2, 0,    0 ]
                               , [ 0, 0, 2,    0 ]
                               , [ 0, 0, 0,    2 ]
                               ]
                               0
    assertBool $ not $ isWon $ build single 0

test_isLost :: IO ()
test_isLost = do
    -- Fără spații și fără vecini egali
    assertBool $ isLost $ build [ [ 2, 4, 2, 4 ]
                                , [ 4, 2, 4, 2 ]
                                , [ 2, 4, 2, 4 ]
                                , [ 4, 2, 4, 2 ]
                                ]
                                0
    -- Un spațiu
    assertBool $ not $ isLost $ build [ [ 2, 4, 2, 4 ]
                                      , [ 4, 0, 4, 2 ]
                                      , [ 2, 4, 2, 4 ]
                                      , [ 4, 2, 4, 2 ]
                                      ]
                                      0
    -- Vecini orizontali
    assertBool $ not $ isLost $ build [ [ 8, 8, 2, 4 ]
                                      , [ 4, 2, 4, 2 ]
                                      , [ 2, 4, 2, 4 ]
                                      , [ 4, 2, 4, 2 ]
                                      ]
                                      0
    -- Vecini verticali
    assertBool $ not $ isLost $ build [ [ 8, 4, 2, 4 ]
                                      , [ 8, 2, 4, 2 ]
                                      , [ 2, 4, 2, 4 ]
                                      , [ 4, 2, 4, 2 ]
                                      ]
                                      0
