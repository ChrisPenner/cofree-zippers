{-# LANGUAGE DeriveFunctor #-}

module Zip where

import Control.Comonad.Cofree
import Data.Functor.Compose

data Zip f a =
  Zip ([Cofree (Compose f Maybe) a])
      (Cofree f a)
  deriving (Functor, Show)

zipCofree :: Cofree f a -> Zip f a
zipCofree fa = Zip [] fa
-- instance (Functor f) => Comonad (Zip f) where
--   extract (Zip _ cfr) = extract cfr
--   duplicate (Zip ps cfr) = Zip undefined (duplicate cfr)
