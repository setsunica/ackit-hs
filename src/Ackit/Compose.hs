{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Ackit.Compose (Compose (..)) where

import Ackit.Data (Text (..), VList (..))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS (unlines, unwords, pack, singleton)
import GHC.Generics (Generic (..), K1 (..), M1 (..), U1, V1, type (:*:) (..), type (:+:) (..))
import qualified GHC.Generics as G

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