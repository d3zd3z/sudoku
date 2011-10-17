-- A simple Sudoku-specific search.

module Sudoku.Simple (
   simpleSolver,

   toFullBoard,
   possibleFromList  -- For Read notation.
) where

import Data.Bits
import Data.List (foldl', minimumBy)
import Data.Ord (comparing)
import qualified Data.Array as A

import Sudoku.Internal

-- Coordinates will be represented as a pair, essentially (y,x),
-- although it doesn't really matter since the boards are reflectable
-- (hence some code below refers to (x,y)).
type Coord = (Int, Int)

-- The board itself will be an array from these Coordinates to
-- a Possible, which is defined below.
type FullBoard = A.Array Coord Possible

-- The array range for the full Sudoku puzzle.
boardRange :: (Coord, Coord)
boardRange = ((1, 1), (boardSize, boardSize))

-- Map a string representation of a board into an array of
-- possibilities for each piece.  The String is assumed to be the
-- correct length.
toFullBoard :: String -> FullBoard
toFullBoard text = A.listArray boardRange $ map singlePiece text

-- For a single piece, return what that means in terms of
-- possibilities.  A fixed value (a digit 1-9) will have only that
-- value as a possibility.  Any other character represents any
-- possibility.
singlePiece :: Char -> Possible
singlePiece ch | ch >= '1' && ch <= '9' =  singlePossible $ read [ch]
singlePiece _ = allPossible

----------------------------------------------------------------------

simpleSolver :: String -> [String]
simpleSolver = solve . toFullBoard

-- Generate all of the solutions to a given FullBoard.  This works by
-- reducing the current board.  If it is a single solution, return it.
-- If there are no solutions, return an empty list.  Otherwise, pick a
-- good location and try each of the possibilities.
solve :: FullBoard -> [String]
solve board = case reduce board of
   board' | isSolved board' -> [getSolution board']
   board' | isImpossible board' -> []
   board' | otherwise ->
      let (cord, pos) = findGood board' in
      let newBoard n = solve $ board' A.// [(cord, singlePossible n)] in
      concatMap newBoard (possibleToList pos)

-- Find a good piece to split the search at.  This is a position that
-- has the fewest possiblities, but still more than one.
findGood :: FullBoard -> (Coord, Possible)
findGood = minimumBy (comparing value) . A.assocs where
   value (_, pos) = case length $ possibleToList pos of
      1 -> maxBound
      n -> n

-- For comparison, we can choose a square with the most possibilities.
-- For a typical-size board, this can take rediculously long.  Given
-- that the problem is NP-complete, this is probably exponential.
-- findGood = minimumBy (comparing (negate . length . possibleToList . snd)) . A.assocs

-- Any time a location on the board lists only a single possibility,
-- we know that all of the pieces in that row/column/block cannot have
-- that value.  Find the fixpoint of the reduction.
reduce :: FullBoard -> FullBoard
reduce board =
   let board' = reduce1 board in
   if board == board' then board
      else reduce board'

-- Run a single reduction.  This may reduce some locations to a single
-- value.
reduce1 :: FullBoard -> FullBoard
reduce1 start = A.array boardRange $ map (newVal start) $ A.assocs start where

-- Reduce a single piece, eliminating any possible choices from it
-- that have known solutions in the same row, column or block.
newVal :: FullBoard -> (Coord, Possible) -> (Coord, Possible)
newVal start (coord, elts) =
   let otherCoord = concat $ groupsOf coord in
   let otherPos = map (singular . (start A.!)) otherCoord in
   (coord, foldl' (/-) elts otherPos)

-- Given a coordinate on the board, return the three lists of
-- coordinates that must be unique.  This function essentially defines
-- the Sudoku problem.  It returns all of the coords in the same row
-- and column, as well as using blockOf to return the same sub-block.
-- All of the lists have the starting coordinate stripped.
groupsOf' :: Coord -> [[Coord]]
groupsOf' ppos@(px, py) = [
   [ (x, py) | x <- [1 .. boardSize], x /= px ],
   [ (px, y) | y <- [1 .. boardSize], y /= py ],
   [ pos     | pos <- blockOf ppos, ppos /= pos ] ]

-- Return a list of the cells in the blockSize*blockSize subblock containing
-- the argument.
blockOf :: Coord -> [Coord]
blockOf (px, py) = [(x, y) | x <- [xbase .. xbase + blockSize - 1],
                    y <- [ybase .. ybase + blockSize - 1]]
   where
      xbase = px - ((px - 1) `mod` blockSize)
      ybase = py - ((py - 1) `mod` blockSize)

-- Memoized version of the above.  Profiling indicates that the
-- program spends about 25% of it's time recomputing the groups.
groupsOf :: Coord -> [[Coord]]
groupsOf pos = cache A.! pos where
   cache = A.array boardRange [ (p, groupsOf' p) | p <- A.range boardRange ]

-- Some general queries about the current state of the board.

-- Has this board reached an impossible situation
isImpossible :: FullBoard -> Bool
isImpossible = any (== impossible) . A.elems

-- Do we have a full solution to this board?
isSolved :: FullBoard -> Bool
isSolved = all ((/= impossible) . singular) . A.elems

-- If isSolved is true, return the solution as a string.
getSolution :: FullBoard -> String
getSolution board =
   map solvedPlace $ A.elems board

-- Convert a possible value back to a character for display.  This is
-- essential the inverse of 'singlePiece'.
solvedPlace :: Possible -> Char
solvedPlace pos = case possibleToList pos of
   [a] -> head $ show a
   _ -> error "Call to solvedPlace on unsolved board"

----------------------------------------------------------------------
-- Representing the possible values.
-- For the smaller sizes (up to at least a 28x28 grid), use bitfields
-- to represent possibilities rather than lists.  The pieces are
-- numbered from 1, so count the first bit in a number as piece '1'.
-- Piece 9 would then be (1 `shiftL` 8) or 256.

newtype Possible = Possible Int
   deriving Eq

-- Having a show instance makes interactive debugging easier.
instance Show Possible where
   show = ("possibleFromList " ++) . show . possibleToList

singlePossible :: Int -> Possible
singlePossible n = Possible $ 1 `shiftL` (n - 1)

-- A square with no possibilities.  Indicates we've reached a dead-end
-- and there is no way to solve this board.
impossible :: Possible
impossible = Possible 0

-- A square that still can hold any value.
allPossible :: Possible
allPossible = Possible $ (1 `shiftL` boardSize) - 1

-- Return a list of the possible values from a given encoded Possible.
possibleToList :: Possible -> [Int]
possibleToList (Possible p)
   | p == 0  = []
   | otherwise =
      let b = p .&. (-p) in
      1 + indexOf b : possibleToList (Possible (p `xor` b))

-- Determine the index of a set bit.  Not really an efficient
-- solution, but it isn't used extensively.  Returns the index of the
-- highest set bit, but should only be called with a single bit set.
indexOf :: Int -> Int
indexOf n | n <= 0 = error "Invalid index operation"
indexOf n = ix n 0 where
   ix 1 i = i
   ix v i = ix (v `shiftR` 1) (i + 1)

-- Convert a list back to a Possible.  Needed to be able to 'read' the
-- value 'show'n.
possibleFromList :: [Int] -> Possible
possibleFromList = Possible . conv where
   conv [] = 0
   conv (a:as) = (1 `shiftL` (a - 1)) .|. conv as

-- Possible set difference.  Remove the values in 'b' from the
-- possibilities in 'a'.
(/-) :: Possible -> Possible -> Possible
(Possible a) /- (Possible b) = Possible $ a .&. complement b

-- If a Possible has only a single possibility, return it, otherwise
-- return the impossible value.
singular :: Possible -> Possible
singular ps@(Possible p) =
   let b = p .&. (-p) in
   if p == b then ps else impossible
