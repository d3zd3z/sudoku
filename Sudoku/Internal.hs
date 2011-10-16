module Sudoku.Internal (
   blockSize, boardSize
) where

-- These converters currently use a rigidly defined board size.  It is
-- defined by the block-size, and the full-board size.
blockSize :: Int
blockSize = 3

boardSize :: Int
boardSize = blockSize * blockSize
