{-# LANGUAGE DoRec #-}
----------------------------------------------------------------------
-- Knuth's Dancing Links solver for Algorithm X.

module Cover.Dlx (
   dlxSolve
) where

import Control.Applicative ((<$>))
import Control.Monad (forM, forM_, zipWithM_)
import Control.Monad.ST
import Data.List (sort)
import Data.STRef
import System.IO.Unsafe
import qualified Data.Map as Map
import qualified Data.Set as Set

import Cover.Types

-- Double linked lists that always have at least one entry.
data DoubleList s a = DoubleList {
   dlItem :: a,
   dlPrior :: (STRef s (DoubleList s a)),
   dlNext :: (STRef s (DoubleList s a)) }

-- From the docs, it appears that Eq on STRef is pointer equality on
-- the reference itself.

singletonD :: a -> ST s (DoubleList s a)
singletonD item = do
   rec
      prior <- newSTRef self
      next <- newSTRef self
      self <- return $ DoubleList item prior next
   return $ self

-- Modify node 'a' chain so that node 'b' follows it.  Any pointers
-- already in 'b' will be removed.
insertAfter :: DoubleList s a -> DoubleList s a -> ST s ()
insertAfter a b = do
   anext <- readSTRef $ dlNext a
   writeSTRef (dlNext a) b
   writeSTRef (dlPrior anext) b
   writeSTRef (dlPrior b) a
   writeSTRef (dlNext b) anext

insertBefore :: DoubleList s a -> DoubleList s a -> ST s ()
insertBefore a b = do
   aprior <- readSTRef $ dlPrior a
   insertAfter aprior b

-- Delete the given node (remove it from it's context), but leave
-- it's pointers alone so it can be reinserted.
deleteD :: DoubleList s a -> ST s ()
deleteD a = do
   prior <- readSTRef $ dlPrior a
   next <- readSTRef $ dlNext a
   writeSTRef (dlNext prior) next
   writeSTRef (dlPrior next) prior

-- Reverse a deleteD.  As long as the context hasn't changed, puts the
-- node back where it was.
reinsertD :: DoubleList s a -> ST s ()
reinsertD a = do
   prior <- readSTRef $ dlPrior a
   next <- readSTRef $ dlNext a
   writeSTRef (dlNext prior) a
   writeSTRef (dlPrior next) a

-- Walk a list, invoking 'op' on each node.  Does not visit the passed
-- in node head.  It is safe to manipulate the pointers within the
-- node passed to 'op'.
walkD :: (a -> ST s ()) -> DoubleList s a -> ST s ()
walkD op nodes = do
   let sentinel = dlNext nodes   -- Use the 'next' pointer STRef to see when done.
   let walk node =
         if dlNext node == sentinel then return ()
            else do
               let item = dlItem node
               next <- readSTRef $ dlNext node
               op item
               walk next
   first <- readSTRef $ dlNext nodes
   walk first

foldrD :: (a -> b -> b) -> b -> DoubleList s a -> ST s b
foldrD op val nodes = do
   let sentinel = dlNext nodes
   let walk node = do
         if dlNext node == sentinel then return val
            else do
               let item = dlItem node
               next <- readSTRef $ dlNext node
               rest <- walk next
               return $ op item rest
   first <- readSTRef $ dlNext nodes
   walk first

-- Pull out the elements of a double-linked list into an ordinary
-- list.
toListD :: DoubleList s a -> ST s [a]
toListD this = do
   let theHead = dlItem this
   items <- foldrD (:) [] this
   return $ theHead : items

-- Pull out the elements, without the header/sentinel node.
toListD' :: DoubleList s a -> ST s [a]
toListD' this = tail <$> toListD this

-- Chain the given nodes together.
chainNodes :: [DoubleList s a] -> ST s ()
chainNodes nodes = do
   case nodes of
      [] -> error "Cannot have empty double-linked lists"
      (a:as) -> do
         forM_ as $ \node -> insertBefore a node

{-
fromListD :: [a] -> ST s (DoubleList s a)
fromListD items = do
   nodes <- mapM singletonD items
   chainNodes nodes
-}

----------------------------------------------------------------------

-- Get an ordered list of all of the rows
allRows :: Ord a => [Column a] -> [a]
allRows = Set.toList . Set.fromList . concatMap colRows

data CellInfo s a
   = Inner { ciRow :: a, ciHeader :: (Cell s (CellInfo s a)) }
   | Header { _hdName :: String, hdSize :: (STRef s Int) }
   | RowHeader a

-- This uses unsafePerformIO to show the count.
instance Show a => Show (CellInfo s a) where
   showsPrec d (Inner a hd) =
      showParen (d > 10) $
         showString "Inner " . showsPrec 11 a . showString " " . showsPrec 11 hd
   showsPrec d (Header name iref) =
      let i = unsafePerformIO $ unsafeSTToIO $ readSTRef iref in
      showParen (d > 1) $
         showString "Header " . showsPrec 11 name . showString " " . showsPrec 11 i
   showsPrec d (RowHeader r) =
      showParen (d > 10) $ showString "RowHeader " . showsPrec 11 r

data Cell s a = Cell {
   cellInfo :: a,
   cellH :: DoubleList s (Cell s a),
   cellV :: DoubleList s (Cell s a) }

-- Show, just shows the info, not the rest.
instance Show a => Show (Cell s a) where
   showsPrec d c =
      showParen (d > 10) $
         showString "Cell " .
         showsPrec 11 (cellInfo c) .
         showString " ###"

-- Create a new cell, linked only to itself
newCell :: a -> ST s (Cell s a)
newCell info = do
   rec
      h <- singletonD cell
      v <- singletonD cell
      cell <- return $! Cell info h v
   return $ cell

type OneCell s a = Cell s (CellInfo s a)

{-
simple :: [Column Int]
simple = [
   makeColumn "D" [2],
   makeColumn "B" [2,3],
   makeColumn "C" [1,3],
   makeColumn "A" [1] ]
-}

buildGrid :: Ord a => [Column a] -> ST s (OneCell s a)
buildGrid  cols = do
   let rows = allRows cols
   rootCount <- newSTRef 0
   root <- newCell $ Header "Root" rootCount
   rowHeads <- mapM (newCell . RowHeader) rows
   chainNodes $ map cellV (root : rowHeads)

   let rowMap = Map.fromList $ zip rows rowHeads

   forM_ cols $ \ (Column { colName = name, colRows = cr }) -> do
      let rheads = map (rowMap Map.!) $ sort cr
      headCount <- newSTRef $ length cr
      colHead <- newCell $ Header name headCount
      cells <- mapM (newCell . flip Inner colHead) cr
      insertBefore (cellH root) (cellH colHead)
      zipWithM_ insertBefore (map cellH rheads) (map cellH cells)
      chainNodes $ map cellV $ colHead : cells

   -- Remove the temporary root headers.
   flip walkD (cellV root) $ \node -> do
      deleteD $ cellV node
      deleteD $ cellH node

   return $ root

data BestResult s a
   = BestSolved
   | BestDeadEnd
   | BestColumn (OneCell s a)
   deriving (Show)

-- Find the best column to try (one with the smallest number of rows).
bestColumn :: OneCell s a -> ST s (BestResult s a)
bestColumn root = do
   cols <- tail <$> (toListD $ cellH root)
   sizes <- mapM (readSTRef . hdSize . cellInfo) cols
   return $ case zip sizes cols of
      [] -> BestSolved
      (a:as) -> scanBest a as
   where
      scanBest :: (Int, OneCell s a) -> [(Int, OneCell s a)] -> BestResult s a
      scanBest (0, _) _ = BestDeadEnd
      scanBest (_, a) [] = BestColumn a
      scanBest a@(asz, _) (b@(bsz, _) : rst)
         | asz <= bsz  = scanBest a rst
         | otherwise  = scanBest b rst

-- Adjust the 'count' of the column containing the specified inner
-- node.  Adds the factor to the count.
adjustSize :: Show a => CellInfo s a -> Int -> ST s ()
adjustSize (Inner _ (Cell (Header _ size) _ _)) count = modifySTRef size (+ count)
adjustSize c _ = error $ "DLX internal error, expecting an inner node: " ++ show c

-- DLX "Cover" of a column
cover :: Show a => OneCell s a -> ST s ()
cover header = do
   case header of
      (Cell (Header {}) _ _) -> return ()
      n -> error $ "cover not called on header: " ++ show n
   deleteD $ cellH header
   colNodes <- toListD' $ cellV header
   forM_ colNodes $ \row -> do
      rowNodes <- toListD' $ cellH row
      forM_ rowNodes $ \node -> do
         deleteD $ cellV node
         adjustSize (cellInfo node) (-1)

-- DLX "Uncover".  Inverts the 'cover' operation.
uncover :: Show a => OneCell s a -> ST s ()
uncover header = do
   case header of
      (Cell (Header {}) _ _) -> return ()
      n -> error $ "cover not called on header: " ++ show n
   colNodes <- reverse <$> (toListD' $ cellV header)
   forM_ colNodes $ \row -> do
      rowNodes <- reverse <$> (toListD' $ cellH row)
      forM_ rowNodes $ \node -> do
         adjustSize (cellInfo node) 1
         reinsertD $ cellV node
   reinsertD $ cellH header

-- Algorithm X using dancing links.  TODO, make this stop on the first
-- solution, rather than finding all of them.
dlx :: Show a => [a] -> Cell s (CellInfo s a) -> ST s [[a]]
dlx ans root = do
   best <- bestColumn root
   case best of
      BestSolved -> return [ans]
      BestDeadEnd -> return []
      BestColumn colHead -> do
         cover colHead
         rows <- toListD' $ cellV colHead
         answers <- forM rows $ \row -> do
            otherCols <- toListD' $ cellH row
            let otherHeads = map (ciHeader . cellInfo) otherCols
            mapM_ cover otherHeads
            result <- dlx ((ciRow $ cellInfo row) : ans) root
            mapM_ uncover $ reverse otherHeads
            return result
         uncover colHead
         return $ concat answers

dlxSolve :: Show a => Ord a => [Column a] -> [[a]]
dlxSolve board =
   runST $ do
      root <- buildGrid board
      dlx [] root
