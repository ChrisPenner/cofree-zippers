{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Lib where

import Control.Applicative
import Control.Comonad.Cofree
import Control.Lens hiding ((:<))
import Data.Functor.Compose
import Data.Maybe
import Zip

extract :: Zip f a -> a
extract (Zip _ (a :< _)) = a

children :: Zip f a -> Cofree f a
children (Zip _ cfr) = cfr

parents :: Zip f a -> [Cofree (Compose f Maybe) a]
parents (Zip ps _) = ps

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

leaf :: Alternative f => a -> Cofree f a
leaf a = a :< empty

exampleRose :: Cofree [] String
exampleRose =
  ("top" :<
   ["l" :< [leaf "ll", leaf "lr"], leaf "m", "r" :< [leaf "rl", leaf "rr"]])

simpleRose :: Cofree [] String
simpleRose = ("top" :< ["l" :< [leaf "end"], leaf "r"])

exampleRoseZip :: Zip [] String
exampleRoseZip = zipCofree exampleRose
