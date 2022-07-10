-- The following URL template is used
-- https://github.com/setsunawb/ackit-hs/blob/main/template/Main.hs
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Main where

import Control.Applicative (Alternative)
import Data.List (elemIndex)
import GHC.Base (Alternative (..))
import GHC.Generics (Generic (..), K1 (..), M1 (..), U1, V1, type (:*:) (..), type (:+:) (..))
import qualified GHC.Generics as G
import Text.Printf (printf)
import Text.Read (readMaybe)


---- Types Module ----

newtype Text = Text String
  deriving (Eq, Ord, Show)

newtype VList a = VList ([] a)
  deriving (Eq, Ord, Show, Functor, Applicative, Monad)

---- Parse Module ----

note :: l -> Maybe r -> Either l r
note l m = case m of
  Nothing -> Left l
  Just v -> Right v

parseErrorNote :: Message -> Maybe a -> P a
parseErrorNote s m = P $ note (ParseError s) m

splitFirstElemSep :: Char -> String -> Maybe (String, String)
splitFirstElemSep c s = do
  idx <- elemIndex c s
  let (b, _ : a) = splitAt idx s
  pure (b, a)

splitFirstLine :: String -> Maybe (String, String)
splitFirstLine = splitFirstElemSep '\n'

splitFirstWord :: String -> Maybe (String, String)
splitFirstWord = splitFirstElemSep ' '

readElem :: (Read a) => String -> Maybe (a, String)
readElem s = pparse <|> pword <|> pline
  where
    pparse = do
      a <- readMaybe s
      pure (a, "")
    pword = do
      (sw1, sw2) <- splitFirstWord s
      a <- readMaybe sw1
      pure (a, sw2)
    pline = do
      (sl1, sl2) <- splitFirstLine s
      a <- readMaybe sl1
      pure (a, sl2)

type Message = String

newtype ParseError = ParseError Message
  deriving (Eq, Ord)

instance Show ParseError where
  show (ParseError msg) = show msg

defaultParseError :: ParseError
defaultParseError = ParseError "an unexpected error occurred"

newtype P a = P (Either ParseError a)
  deriving (Eq, Ord, Show, Functor, Applicative, Monad)

instance Alternative P where
  empty = P $ Left defaultParseError
  (<|>) l r
    | P (Right _) <- l = l
    | otherwise = r

parseErrorP :: Message -> P a
parseErrorP s = P $ Left $ ParseError s

class Parse a where
  parse :: String -> P (a, String)
  default parse :: (Generic a, Parse' (Rep a)) => String -> P (a, String)
  parse s = do
    (rep, s') <- parse' s
    pure (G.to rep, s')

instance Parse Text where
  parse s = parseErrorNote msg $ fline <|> fword
    where
      msg = printf "failed to parse Text [%s]" s
      fline = do
        (s1, s2) <- splitFirstLine s
        pure (Text s1, s2)
      fword = do
        (s1, s2) <- splitFirstWord s
        pure (Text s1, s2)

instance Parse Int where
  parse s = parseErrorNote msg $ readElem s
    where
      msg = printf "failed to parse Int [%s]" s

instance Parse Double where
  parse s = parseErrorNote msg $ readElem s
    where
      msg = printf "failed to parse Double [%s]" s

instance Parse Char where
  parse [] = parseErrorP "failed to parse char because string is empty"
  parse (c : s) = pure (c, s)

instance Parse a => Parse [a] where
  parse s = do
    (s1, s2) <- parseErrorNote msg $ splitFirstLine s
    (acmx, _) <- f ([], s1)
    pure (acmx, s2)
    where
      msg = printf "failed to parse List [%s]" s
      f :: Parse a => ([a], String) -> P ([a], String)
      f (acmx, []) = pure (reverse acmx, [])
      f (acmx, s') = do
        (a, s'') <- parse s'
        f (a : acmx, s'')

instance Parse a => Parse (VList a) where
  parse s = do
    (acmx, s') <- f ([], s)
    pure (VList acmx, s')
    where
      f :: Parse a => ([a], String) -> P ([a], String)
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
  parse' :: String -> P (f a, String)

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

---- Compose Module ----

class Compose a where
  compose :: a -> String
  default compose :: (Generic a, Compose' (Rep a)) => a -> String
  compose = compose' . G.from

instance Compose Int where
  compose = show

instance Compose Double where
  compose = show

instance Compose Char where
  compose = pure

instance Compose Text where
  compose (Text s) = s

instance (Compose a) => Compose [a] where
  compose l = unwords $ compose <$> l

instance (Compose a) => Compose (VList a) where
  compose (VList l) = unlines $ compose <$> l

instance (Compose a, Compose b) => Compose (a, b) where
  compose (a, b) = compose a <> " " <> compose b

class Compose' f where
  compose' :: f a -> String

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
    d = div k 60
    m = mod k 60
    h = 21 + d
    answer = A . Text $ printf "%d:%02d" h m

main :: IO ()
main = interact overhaul
  where
    overhaul s = case parse s of
      P (Right (q, _)) -> compose . solve $ q
      P (Left err) -> show err