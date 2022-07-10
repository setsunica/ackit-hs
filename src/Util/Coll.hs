module Util.Coll where
import qualified GHC.Arr as A
import GHC.Arr (Array)
import Util.Data (Coord (..))

nat :: [Int]
nat = iterate (+ 1) 0

indexed :: [a] -> [(Int, a)]
indexed = zip nat

coordArray :: (Int, Int) -> [[a]] -> Array (Coord Int) a
coordArray (n, m) nl = A.array range al
  where
    range = (Coord (0, 0), Coord (n, m))
    inl = indexed nl
    al = do
      (i, r) <- inl
      let ir = indexed r
      (j, c) <- ir
      pure (Coord (i, j), c)

data Direc = R | UR | T | UL | L | LL | B | LR
  deriving (Eq, Ord, Show)

direcs :: [Direc]
direcs = [R, UR, T, UL, L, LL, B, LR]

dv :: Direc -> Coord Int
dv d = case d of
  R -> Coord (1, 0)
  UR -> Coord (1, 1)
  T -> Coord (0, 1)
  UL -> Coord (-1, 1)
  L -> Coord (-1, 0)
  LL -> Coord (-1, -1)
  B -> Coord (0, -1)
  LR -> Coord (1, -1)