{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Ackit.Parse (P (..), Parse (..), ParseError, splitFirstLine, splitFirstWord, runP) where

import Ackit.Data (Text (..), VList (..))
import Control.Applicative (Alternative, (<|>))
import Control.Monad.Trans.Except (Except, except, runExcept)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (break, uncons, unpack, dropSpace)
import GHC.Generics as G (Generic (..), K1 (..), M1 (..), U1, V1, (:*:) (..), (:+:) (..))
import qualified Text.Printf as T (printf)
import qualified Text.Read as T (readMaybe)

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

readElem ::Read a => ByteString -> Maybe (a, ByteString)
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
