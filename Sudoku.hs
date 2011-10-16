-- Board builders and support for Sudoku.

module Sudoku (
   readBoards,
   runSolver,

   module Sudoku.Simple,
   module Sudoku.Cover
) where

import Control.Arrow (second)
import Control.Monad (forM_, liftM)
import Data.List (intercalate)

import Sudoku.Internal
import Sudoku.Simple
import Sudoku.Cover

-- Project Euler, problem 96, has a set of sudoku problems in a simple
-- text file.  After converting to local line-ending format, this will
-- read this puzzle file in to a list of strings defining the
-- problems.  Each puzzle also has a name.
readBoards :: FilePath -> IO [(String, String)]
readBoards = liftM (convertBoards . lines) . readFile

convertBoards :: [String] -> [(String, String)]
convertBoards [] = []
convertBoards (name : rest) = case splitAt boardSize rest of
   (thisBoard, more) -> (name, concat thisBoard) : convertBoards more

runSolver :: (String -> [String]) -> IO ()
runSolver solve = do
   boards <- readBoards "sudoku.txt"
   forM_ (map (second solve) boards) $ \(name, solves) -> do
      putStrLn name
      let solves' = map (intercalate "\n" . splitUp boardSize) solves
      putStrLn $ intercalate "\n---\n" solves' ++ "\n---"

-- Utility, split a list into chunks of the given size.
splitUp :: Int -> [a] -> [[a]]
splitUp n ary = case splitAt n ary of
   ([], _) -> []
   (chunk, rest) -> chunk : splitUp n rest
