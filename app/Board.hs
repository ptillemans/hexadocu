-- | Hexadocu board 

module Board (
  Column,
  mkColumn,
  Row,
  mkRow,
  Cell(..),
  CellValue(..),
  Board(..)
             ) where

import Data.Map as M

data Column = Column Int deriving (Show)

--
-- Create typesafe columns
--
-- >>> mkColumn 4 
-- Column 4
--
mkColumn :: Int -> Column
mkColumn n
        | n < 0 || n > 15 = error "Invalid column"
        | otherwise = Column n


data Row = Row Int deriving (Show)

mkRow :: Int -> Row
mkRow n
  | n < 0 || n > 15 = error "Invalid row"
  | otherwise = Row n


data Cell = C0 | C1 | C2 | C3
  | C4 | C5 | C6 | C7
  | C8 | C9 | CA | CB
  | CC | CD | CE | CF
  deriving (Enum)

instance Show Cell where
  show C0 = "0"
  show C1 = "1"
  show C2 = "2"
  show C3 = "3"
  show C4 = "4"
  show C5 = "5"
  show C6 = "6"
  show C7 = "7"
  show C8 = "8"
  show C9 = "9"
  show CA = "A"
  show CB = "B"
  show CC = "C"
  show CD = "D"
  show CE = "E"
  show CF = "F"
  
instance Read Cell where
  readsPrec _ s
        | s == show C0 = [(C0, "")]
        | s == show C1 = [(C1, "")]
        | s == show C2 = [(C2, "")]
        | s == show C3 = [(C3, "")]
        | s == show C4 = [(C4, "")]
        | s == show C5 = [(C5, "")]
        | s == show C6 = [(C6, "")]
        | s == show C7 = [(C7, "")]
        | s == show C8 = [(C8, "")]
        | s == show C9 = [(C9, "")]
        | s == show CA = [(CA, "")]
        | s == show CB = [(CB, "")]
        | s == show CC = [(CC, "")]
        | s == show CD = [(CD, "")]
        | s == show CE = [(CE, "")]
        | s == show CF = [(CF, "")]
        | otherwise = []


data CellValue = Empty | Cell | List Cell

-- >>> :t (1,2)   
-- (1,2) :: (Num a, Num b) => (a, b)

type Board = M.Map (Row, Column) CellValue


