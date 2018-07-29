{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Lib where

import Control.Comonad.Cofree
import Control.Lens hiding ((:<))
import Data.Functor.Compose
import Data.Maybe

data Zip f a =
  Zip ([Cofree (Compose f Maybe) a])
      (Cofree f a)
  deriving (Functor, Show)

-- deriving instance (Show1 f, Show a) => Show (Zip f a)
zipCofree :: Cofree f a -> Zip f a
zipCofree fa = Zip [] fa

extract :: Zip f a -> a
extract (Zip _ (a :< _)) = a

up ::
     forall f a. Functor f
  => Zip f a
  -> Maybe (Zip f a)
up (Zip [] _) = Nothing
up (Zip ((p :< pcfr):ps) cfr) = Just $ Zip ps newFocus
  where
    newFocus :: Cofree f a
    newFocus = p :< blah pcfr
    blah :: Compose f Maybe (Cofree (Compose f Maybe) a) -> f (Cofree f a)
    blah (Compose fm) = fmap blah2 fm
    blah2 :: Maybe (Cofree (Compose f Maybe) a) -> Cofree f a
    blah2 Nothing = cfr
    blah2 (Just (a :< as)) = a :< blah as

up' :: Functor f => Zip f a -> Zip f a
up' z = fromMaybe z (up z)

hoistMaybe :: Functor f => Cofree f a -> Cofree (Compose f Maybe) a
hoistMaybe = hoistCofree (Compose . fmap pure)

down ::
     forall f a. Functor f
  => (forall b. Traversal' (f b) b)
  -> Zip f a
  -> Maybe (Zip f a)
down t (Zip oldParents cfr@(_ :< fa)) =
  case (fa ^? t) of
    Nothing -> Nothing
    Just next -> Just $ Zip (newParent : oldParents) next
  where
    newParent :: Cofree (Compose f Maybe) a
    newParent = setP $ hoistMaybe cfr
    setP :: Cofree (Compose f Maybe) a -> Cofree (Compose f Maybe) a
    setP (a :< Compose fa') = a :< Compose (set t Nothing fa')

down' :: Functor f => (forall b. Traversal' (f b) b) -> Zip f a -> Zip f a
down' t z = fromMaybe z (down t z)

exampleM :: Zip Maybe Char
exampleM = zipCofree ('a' :< Just ('b' :< Just ('c' :< Nothing)))

exampleL :: Zip [] Char
exampleL = zipCofree ('a' :< ['b' :< ['d' :< []], 'c' :< []])

exampleCofree :: Cofree [] Char
exampleCofree = ('a' :< ['b' :< ['d' :< []], 'c' :< []])
