{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Util.Compose (Compose (..)) where

import GHC.Generics (U1, V1, type (:+:) (..), type (:*:) (..), K1 (..), M1 (..), Generic (..))
import Util.Data (Text (..), VList (..))
import qualified GHC.Generics as G

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