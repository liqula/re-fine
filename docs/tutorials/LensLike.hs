{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module LensLike where

import Prelude hiding ((.), id, curry, uncurry, const)
import Control.Monad (join)
import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Traversable as Traversable
import Data.Functor.Compose

import Control.Lens as Lens

-- Compose f g a = Compose (f (g a))
type C = Compose
type MonadLens m f a b = LensLike (C m f) a a b b

{-
type LensLike (f :: k -> *) s (t :: k) a (b :: k) =
  (a -> f b) -> s -> f t
-}

data ID a = ID

data User = User
  { _firstName :: String
  , _lastName  :: String
  }

makeLenses ''User

loadUser' :: ID User -> IO User
loadUser' uid = pure $ User "f" "l"

updateUser :: ID User -> User -> IO ()
updateUser uid u = pure ()

type PersistLens backend a b
  = forall f . (Functor f, Traversable f)
  => MonadLens backend f a b

getDB :: Monad m => MonadLens m (Const a) b a -> b -> m a
getDB c = fmap (fmap getConst . getCompose) . c $ (Compose . pure . Const)

modifyDB :: Monad m => MonadLens m Identity b a -> (a -> m a) -> b -> m b
modifyDB c f = fmap (fmap runIdentity . getCompose). c $ (Compose . fmap Identity . f)

modifyDB' :: Monad m => MonadLens m Identity b a -> (a -> a) -> b -> m b
modifyDB' c f = modifyDB c (pure . f)

user :: PersistLens IO (ID User) User
user kuu uid = Compose $ do
  u <- loadUser' uid
  fu <- getCompose (kuu u)
  tu <- Traversable.mapM (updateUser uid) fu
  pure $ (\_ -> uid) <$> tu

lh :: Monad m => Lens' a b -> PersistLens m a b
lh l kbb a = Compose $ do
  fb <- getCompose (kbb (a ^. l))
  pure $ (\b -> set l b a) <$> fb

test :: PersistLens IO (ID User) String
test = user . firstName
