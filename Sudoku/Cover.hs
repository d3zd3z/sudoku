----------------------------------------------------------------------

module Sudoku.Cover (
   pureCoverSolver,
   fastPureCoverSolver
) where

import Data.List (sort)
import Data.Maybe (catMaybes)

import Cover (makeColumn, Column, fastPureSolve, pureSolve)
import Sudoku.Internal

-- Use 10s to make the rows easy to read.  To solve bigger puzzles,
-- this factor would need to be larger.
makeRow :: Int -> Int -> Int -> Int
makeRow row col piece = 100*row + 10*col + piece

unmakeRow :: Int -> (Int, Int, Int)
unmakeRow coded =
   let (row, c2) = coded `divMod` 100 in
   let (col, piece) = c2 `divMod` 10 in
   (row, col, piece)

-- An unconstrained sudoku board of size nsqrt^2 wide and nsqrt^2 high
-- has the following constraints.  There is a single row for each
-- possible piece in each possible squre.
boardConstraints :: Int -> [Column Int]
boardConstraints nsqrt =
   let n = nsqrt * nsqrt in
   -- Each cell can only have a single number.
   [ makeColumn ("u" ++ show row ++ show col)
      [ makeRow row col piece | piece <- [1..n] ]
      | row <- [1..n], col <- [1..n] ] ++
   -- Each number only once per row.
   [ makeColumn ("r" ++ show row ++ "-p" ++ show piece)
      [ makeRow row col piece | col <- [1..n] ]
      | row <- [1..n], piece <- [1..n] ] ++
   -- Each number only once per column.
   [ makeColumn ("c" ++ show col ++ "-p" ++ show piece)
      [ makeRow row col piece | row <- [1..n] ]
      | col <- [1..n], piece <- [1..n] ] ++
   -- Each number must only occur once in each group.
   [ makeColumn ("g" ++ show (gx+1) ++ show (gy+1) ++ "-p" ++ show piece)
      [ makeRow ((gy*nsqrt) + dy + 1) ((gx*nsqrt) + dx + 1) piece
         | dy <- [0 .. nsqrt-1], dx <- [0 .. nsqrt-1] ]
      | gy <- [0 .. nsqrt-1], gx <- [0 .. nsqrt-1], piece <- [1..n] ]

-- A given problem statement can be represented as additional columns
-- with just a single row listed.
givenConstraints :: String -> [Column Int]
givenConstraints text =
   let n = floor (sqrt $ fromIntegral (length text) :: Double) in
   let cells = [ (row, col) | row <- [1..n], col <- [1..n] ] in
   catMaybes $ zipWith collect text cells
   where
      collect ch (row, col)
         | ch >= '1' && ch <= '9' =
            let piece = fromEnum ch - fromEnum '0' in
            Just $ makeColumn ("g" ++ show row ++ show col ++ "-p" ++ show piece)
               [ makeRow row col piece ]
         | otherwise = Nothing

-- Convert an answer (as a list of Rows) back into a problem
-- statement.
answerToBoard :: [Int] -> String
answerToBoard = concatMap decodeRow . sort where
   decodeRow row = let (_, _, piece) = unmakeRow row in show piece

pureCoverSolver :: String -> [String]
pureCoverSolver board = map answerToBoard $ pureSolve $ boardConstraints blockSize ++ givenConstraints board

fastPureCoverSolver :: String -> [String]
fastPureCoverSolver board = map answerToBoard $ fastPureSolve $ boardConstraints blockSize ++ givenConstraints board
