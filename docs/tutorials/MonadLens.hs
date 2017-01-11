{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module MonadLens where

import Prelude hiding ((.), id, curry, uncurry, const)
import Control.Monad (join)
import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Traversable as Traversable

import Control.Lens as Lens

-- Laarhoven lens based on categories.
newtype MonadLens m f a b = MonadLens {
    clens :: (b -> m (f b)) -> a -> m (f a)
  }

instance Category (MonadLens m f) where
  id                            = MonadLens id
  (MonadLens f) . (MonadLens g) = MonadLens (g . f)


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

get :: Monad m => MonadLens m (Const a) b a -> b -> m a
get (MonadLens c) = fmap (fmap getConst) . c $ (pure . Const)

modify :: Monad m => MonadLens m Identity b a -> (a -> m a) -> b -> m b
modify (MonadLens c) f = fmap (fmap runIdentity) . c $ (fmap Identity . f)

modify' :: Monad m => MonadLens m Identity b a -> (a -> a) -> b -> m b
modify' c f = modify c (pure . f)

user :: PersistLens IO (ID User) User
user = MonadLens $ \kuu uid -> do
  u  <- loadUser' uid
  fu <- kuu u
  tu <- Traversable.mapM (updateUser uid) fu
  pure $ (\_ -> uid) <$> tu

lh :: Monad m => Lens' a b -> PersistLens m a b
lh l = MonadLens $ \kbb a -> do
  fb <- kbb (a ^. l)
  pure $ (\b -> set l b a) <$> fb

test :: PersistLens IO (ID User) String
test = lh firstName . user
