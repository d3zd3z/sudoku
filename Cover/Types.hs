module Cover.Types (
   Column(..),
   Board,
   makeColumn
) where

data Column a = Column { colName :: String, columnSize :: !Int, colRows :: [a] }
   deriving (Eq, Show)

type Board = [Column Int]

makeColumn :: String -> [a] -> Column a
makeColumn name rows = Column name (length rows) rows

