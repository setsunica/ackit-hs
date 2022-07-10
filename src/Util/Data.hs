{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Util.Data(Text(..), VList(..)) where

newtype Text = Text String
  deriving (Eq, Ord, Show)

newtype VList a = VList ([] a)
  deriving (Eq, Ord, Show, Functor, Applicative , Monad)