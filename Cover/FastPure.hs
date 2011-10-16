-- A more optimized pure Algorithm X solver.

module Cover.FastPure (
   fastPureSolve
) where

import Data.List (minimumBy, partition)
import Data.IntSet (IntSet)
import Data.Ord (comparing)
import qualified Data.IntSet as IntSet

import Cover.Types

-- We make two improvements over the naive solution.  First, we use an
-- IntSet to hold the rows left in each column.  This allows faster
-- set operations to be performed.  Second, we use a selection
-- algorithm rather than a full sort of the columns each time.
-- My benchmarks show this to be about 40% faster than the naive
-- code.

data FColumn = FColumn { fcolName :: String, fcolRows :: IntSet }

toFColumn :: Column Int -> FColumn
toFColumn (Column { colName = name, colRows = rows }) =
   FColumn { fcolName = name, fcolRows = IntSet.fromList rows }

hasRow :: Int -> FColumn -> Bool
hasRow row (FColumn _ rows) = IntSet.member row rows

removeRows :: IntSet -> FColumn -> FColumn
removeRows toRemove (FColumn name rows) =
   FColumn name (IntSet.difference rows toRemove)

fastPureSolve :: [Column Int] -> [[Int]]
fastPureSolve = pureSolve [] . map toFColumn

pureSolve :: [Int] -> [FColumn] -> [[Int]]
pureSolve ans board = case selectBy (comparing $ IntSet.size . fcolRows) board of
   [] -> [ans]   -- All constraints used, solved.
   (FColumn _ rows : _) | IntSet.null rows -> []  -- Dead end.
   (FColumn _ rows : cs) ->
      let tryRow row =
            let (delCols, keepCols) = partition (hasRow row) cs in
            let delRows = IntSet.unions $ map fcolRows delCols in
            let newCols = map (removeRows delRows) keepCols in
            pureSolve (row:ans) newCols in
      concatMap tryRow $ IntSet.toList rows

-- Selection sort, make sure that an element with the smallest value
-- of 'p' is at the head of the list.  Partitioning by the selection
-- criteria is important (rather than equality), since equality may be
-- expensive.
selectBy :: (a -> a -> Ordering) -> [a] -> [a]
selectBy _ [] = []
selectBy p l = smalls ++ rest where
   best = minimumBy p l
   (smalls, rest) = partition isBest l
   isBest b = (p best b) == EQ
