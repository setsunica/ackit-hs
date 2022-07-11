-- The following URL template is used
-- https://github.com/setsunawb/ackit-hs/blob/main/template/Main.hs (v0.2.0.0)
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Main (main) where

import Control.Applicative (Alternative)
import Control.Monad.Trans.Except (Except, except, runExcept)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import GHC.Arr (Array, Ix)
import qualified GHC.Arr as A (array)
import GHC.Base (Alternative (..), divModInt)
import GHC.Generics (Generic (..), K1 (..), M1 (..), U1, V1, type (:*:) (..), type (:+:) (..))
import qualified GHC.Generics as G
import qualified Text.Printf as T
import qualified Text.Read as T

---- Ackit.Data Module ----

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

---- Ackit.Parse Module ----

note :: l -> Maybe r -> Either l r
note l m = case m of
  Nothing -> Left l
  Just v -> Right v

splitFirstElemSpace :: Char -> ByteString -> (ByteString, ByteString)
splitFirstElemSpace c s = BS.dropSpace <$> BS.break (== c) s

splitFirstLine :: ByteString -> (ByteString, ByteString)
splitFirstLine = splitFirstElemSpace '\n'

splitFirstWord :: ByteString -> (ByteString, ByteString)
splitFirstWord = splitFirstElemSpace ' '

readElem :: Read a => ByteString -> Maybe (a, ByteString)
readElem s = pword <|> pline <|> pparse
  where
    pword = do
      let (sw1, sw2) = splitFirstWord s
      a <- T.readMaybe $ BS.unpack sw1
      pure (a, sw2)
    pline = do
      let (sl1, sl2) = splitFirstLine s
      a <- T.readMaybe $ BS.unpack sl1
      pure (a, sl2)
    pparse = do
      a <- T.readMaybe $ BS.unpack s
      pure (a, "")

type Message = String

newtype ParseError = ParseError Message
  deriving (Eq, Ord, Semigroup, Monoid)

instance Show ParseError where
  show (ParseError msg) = show msg

newtype P a = P (Except ParseError a)
  deriving (Eq, Ord, Show, Functor, Applicative, Alternative, Monad)

runP :: P a -> Either ParseError a
runP (P e) = runExcept e

noteErrP :: Message -> Maybe a -> P a
noteErrP s = P . except . note (ParseError s)

class Parse a where
  parse :: ByteString -> P (a, ByteString)
  default parse :: (Generic a, Parse' (Rep a)) => ByteString -> P (a, ByteString)
  parse s = do
    (rep, s') <- parse' s
    pure (G.to rep, s')

instance Parse Text where
  parse s = pure (Text $ BS.unpack s1, s2)
    where
      (s1, s2) = splitFirstLine s

instance Parse Int where
  parse s = noteErrP msg $ readElem s
    where
      msg = T.printf "failed to parse Int [%s]" $ BS.unpack s

instance Parse Double where
  parse s = noteErrP msg $ readElem s
    where
      msg = T.printf "failed to parse Double [%s]" $ BS.unpack s

instance Parse Char where
  parse = noteErrP msg . BS.uncons
    where
      msg = "failed to parse char because string is empty"

instance Parse a => Parse [a] where
  parse s = do
    (acmx, _) <- f ([], s1)
    pure (acmx, s2)
    where
      (s1, s2) = splitFirstLine s
      f :: Parse a => ([a], ByteString) -> P ([a], ByteString)
      f (acmx, []) = pure (reverse acmx, [])
      f (acmx, s') = do
        (a, s'') <- parse s'
        f (a : acmx, s'')

instance Parse a => Parse (VList a) where
  parse s = do
    (acmx, s') <- f ([], s)
    pure (VList acmx, s')
    where
      f :: Parse a => ([a], ByteString) -> P ([a], ByteString)
      f (acmx, []) = pure (reverse acmx, [])
      f (acmx, s') = do
        (a, s'') <- parse s'
        f (a : acmx, s'')

instance (Parse a, Parse b) => Parse (a, b) where
  parse s = do
    (l, s') <- parse s
    (r, s'') <- parse s'
    pure ((l, r), s'')

class Parse' f where
  parse' :: ByteString -> P (f a, ByteString)

instance Parse' V1 where
  parse' = undefined

instance Parse' U1 where
  parse' = undefined

instance (Parse' f, Parse' g) => Parse' (f :+: g) where
  parse' s = lp <|> rp
    where
      lp = do
        (l1, s') <- parse' s
        pure (L1 l1, s')
      rp = do
        (r1, s') <- parse' s
        pure (R1 r1, s')

instance (Parse' f, Parse' g) => Parse' (f :*: g) where
  parse' s = do
    (l, s') <- parse' s
    (r, s'') <- parse' s'
    pure (l :*: r, s'')

instance (Parse c) => Parse' (K1 i c) where
  parse' s = do
    (k1, s') <- parse s
    pure (K1 k1, s')

instance (Parse' f) => Parse' (M1 i t f) where
  parse' s = do
    (m1, s') <- parse' s
    pure (M1 m1, s')

---- Ackit.Compose Module ----

class Compose a where
  compose :: a -> ByteString
  default compose :: (Generic a, Compose' (Rep a)) => a -> ByteString
  compose = compose' . G.from

instance Compose Int where
  compose = BS.pack . show

instance Compose Double where
  compose = BS.pack . show

instance Compose Char where
  compose = BS.singleton

instance Compose Text where
  compose (Text s) = BS.pack s

instance (Compose a) => Compose [a] where
  compose l = BS.unwords $ compose <$> l

instance (Compose a) => Compose (VList a) where
  compose (VList l) = BS.unlines $ compose <$> l

instance (Compose a, Compose b) => Compose (a, b) where
  compose (a, b) = compose a <> " " <> compose b

class Compose' f where
  compose' :: f a -> ByteString

instance Compose' V1 where
  compose' = undefined

instance Compose' U1 where
  compose' = undefined

instance (Compose' f, Compose' g) => Compose' (f :+: g) where
  compose' (L1 x) = compose' x
  compose' (R1 x) = compose' x

instance (Compose' f, Compose' g) => Compose' (f :*: g) where
  compose' (f :*: g) = compose' f <> "\n" <> compose' g

instance Compose c => Compose' (K1 i c) where
  compose' (K1 x) = compose x

instance Compose' f => Compose' (M1 i t f) where
  compose' (M1 x) = compose' x

---- Ackit.Coll Module ----

indexed :: [a] -> [(Int, a)]
indexed = zip [1 ..]

coordArray :: (Int, Int) -> [[a]] -> Array (Coord Int) a
coordArray (n, m) nl = A.array range al
  where
    range = (Coord (1, 1), Coord (n, m))
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

---- Main  Module ----

data Q = Q Int
  deriving (Generic)

instance Parse Q

data A = A Text
  deriving (Generic)

instance Compose A

solve :: Q -> A
solve (Q k) = answer
  where
    (d, m) = divModInt k 60
    h = 21 + d
    answer = A . Text $ T.printf "%d:%02d" h m

main :: IO ()
main = BS.interact overhaul
  where
    overhaul s = case runP . parse $ s of
      (Right (q, _)) -> compose . solve $ q
      (Left err) -> BS.pack $ show err