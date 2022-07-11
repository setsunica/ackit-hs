{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ackit.Data (Text (..), VList (..), Coord (..)) where

import Data.Ix (Ix)

newtype Text = Text String
  deriving (Eq, Ord, Show)

newtype VList a = VList ([] a)
  deriving (Eq, Ord, Show, Functor, Applicative, Monad)

newtype Coord a = Coord (a, a)
  deriving (Eq, Ord, Show, Ix)

instance Num a => Num (Coord a) where
  (Coord (x1, y1)) + (Coord (x2, y2)) = Coord (x1 + x2, y1 + y2)
  (Coord (x1, y1)) * (Coord (x2, y2)) = Coord (x1 * x2, y1 * y2)
  abs (Coord (x, y)) = Coord (abs x, abs y)
  signum (Coord (x, y)) = Coord (signum x, signum y)
  fromInteger _ = undefined
  negate (Coord (x, y)) = Coord (-x, -y)