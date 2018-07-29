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
import Data.Functor.Classes
import Data.Functor.Compose

newtype Fix1 f a =
  Fix1 (f (Fix1 f) a)

data Parent f a
  = Parent (Cofree (Compose f (Either ())) a)
  | NoParent
  deriving (Functor, Show)

-- instance Functor f => Functor (Parent f) where
--   fmap :: forall a b. (a -> b) -> Parent f a -> Parent f b
--   fmap _ NoParent = NoParent
--   fmap f (Parent x) = Parent $ mapInner x
--     where
--       mapInner ::
--            (Functor f)
--         => f (Either a (Cofree f (Parent f a)))
--         -> f (Either b (Cofree f (Parent f b)))
--       mapInner = fmap (bimap f (fmap (fmap f)))
-- type Parent f a = Maybe (f (Either a (Cofree f)))
-- type Parent f a = Fix1 (Compose (Compose Maybe (Cofree f))) a
data Zip f a =
  Zip (Parent f a)
      (Cofree f a)
  deriving (Functor)

deriving instance (Show1 f, Show a) => Show (Zip f a)

zipCofree :: Cofree f a -> Zip f a
zipCofree fa = Zip NoParent fa

extract :: Zip f a -> a
extract (Zip _ (a :< _)) = a

-- down ::
--      forall f a. Alternative f
--   => (forall b. Traversal' (f b) b)
--   -> Zip f a
--   -> Zip f a
-- down t z@(Zip oldParent (a :< fa)) =
--   case (fa ^? t) of
--     Nothing -> z
--     Just next -> Zip newParent next
--   where
--     newParent :: Parent f a
--     newParent =
--       case oldParent of
--         NoParent -> Parent (set t (a :< empty) fa)
--         Parent oldParent' -> Parent $ set t (a :< oldParent') fa
hoistEither :: Functor f => f a -> Compose f (Either ()) a
hoistEither = Compose . fmap Right

down ::
     forall f a. Alternative f
  => (forall b. Traversal' (f b) b)
  -> Zip f a
  -> Zip f a
down t z@(Zip oldParent (a :< fa)) =
  case (fa ^? t) of
    Nothing -> z
    Just next -> Zip newParent next
  where
    newParent :: Parent f a
    newParent =
      case oldParent of
        NoParent ->
          Parent $
          hoistCofree (Compose . set t (Left ()) . getCompose) $
          hoistCofree hoistEither (a :< fa)
        Parent oldParent' ->
          let newLevel = hoistCofree hoistEither (a :< fa)
           in Parent $ embed newLevel oldParent'

embed ::
     Functor f
  => Cofree (Compose f (Either ())) a
  -> Cofree (Compose f (Either ())) a
  -> Cofree (Compose f (Either ())) a
embed newLevel (a :< Compose fa) =
  (a :< Compose (fmap (fmap (embed newLevel)) (switchLefts newLevel <$> fa)))
  where
    switchLefts :: forall b. b -> Either () b -> Either () b
    switchLefts a' (Left ()) = Right a'
    switchLefts _ r = r

-- up :: Zip Maybe a -> Zip Maybe a
-- up z@(Zip Nothing _ _) = z
-- up (Zip (Just (pa :< pfa)) a fa) = Zip newParent pa (pure (a :< fa))
--   where
--     newParent :: Maybe (Cofree (Maybe (Either ())) a)
--     newParent = either (const pfa) id <$> pfa
exampleM :: Zip Maybe Char
exampleM = zipCofree ('a' :< Just ('b' :< Just ('c' :< Nothing)))

exampleL :: Zip [] Char
exampleL = zipCofree ('a' :< ['b' :< ['d' :< []], 'c' :< []])
