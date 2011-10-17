-- Sudoku solvers.

module Main where

import Control.Monad (forM_)
import Data.Map (Map)
import System.Environment
import qualified Data.Map as Map

import Sudoku

main :: IO ()
main = do
   args <- getArgs
   case args of
      [name] -> case Map.lookup name solvers of
         Just solver -> runSolver solver
         Nothing -> usage
      _ -> usage

usage :: IO ()
usage = do
   pname <- getProgName
   putStrLn $ "Usage: " ++ pname ++ " solver"
   putStrLn ""
   putStrLn "Solvers:"
   forM_ (Map.keys solvers) $ \name -> do
      putStrLn $ "   " ++ name

solvers :: Map String (String -> [String])
solvers = Map.fromList [
   ("simple", simpleSolver),
   ("pureCover", pureCoverSolver),
   ("fastPureCover", fastPureCoverSolver),
   ("dlx", dlxSolver) ]
