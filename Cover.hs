-- Solvers for the exact cover problem.

module Cover (
   module Cover.Types,

   pureSolve,
   fastPureSolve,
   dlxSolve
) where

import Data.List (partition, sortBy)
import Data.Ord (comparing)
import qualified Data.Set as Set

import Cover.Types
import Cover.FastPure
import Cover.Dlx

----------------------------------------------------------------------
-- Although Knuth's DLX implements Algorithm X using mutable state,
-- let's implement Algorithm X as pure code first.  This will give us
-- something to compare with.

-- Does this column contain the particular row.
{-# SPECIALIZE hasRow :: Int -> Column Int -> Bool #-}
hasRow :: Eq a => a -> Column a -> Bool
hasRow row (Column _ _ rows) = elem row rows

-- Apply filter on the predicate for the row.
filterRows :: (a -> Bool) -> Column a -> Column a
filterRows f (Column name _ rows) = makeColumn name $ filter f rows

pureSolve :: Ord a => [Column a] -> [[a]]
pureSolve = pureSolve' []

{-# SPECIALIZE pureSolve' :: [Int] -> [Column Int] -> [[Int]] #-}
pureSolve' :: Ord a => [a] -> [Column a] -> [[a]]
pureSolve' ans board = case sortBy (comparing columnSize) board of
   [] -> [ans]  -- All constraints used, solved.
   (Column _ _ [] : _) -> []  -- Dead end.
   (Column _ _ rows : cs) ->
      let tryRow row =
            let (delCols, keepCols) = partition (hasRow row) cs in
            let delRows = Set.fromList $ concatMap colRows delCols in
            let newCols = map (filterRows (`Set.notMember` delRows)) keepCols in
            pureSolve' (row:ans) newCols in
      concatMap tryRow rows
