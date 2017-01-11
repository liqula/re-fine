{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module CatLens where

import Prelude hiding ((.), id, curry, uncurry, const)
import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Traversable as Traversable

import Control.Lens as Lens

-- Laarhoven lens based on categories.
newtype CatLens cat f a b = CatLens {
    clens :: cat (cat b (f b), a) (f a)
  }

const :: Arrow arr => c -> arr b c
const a = arr (\_ -> a)

curry :: Arrow cat => cat (a, b) c -> (a -> cat b c)
curry m a = m . (const a &&& id)

uncurry :: ArrowApply cat => (a -> cat b c) -> cat (a, b) c
uncurry a = app . arr (first a)

instance (ArrowApply cat) => Category (CatLens cat f) where
  id                        = CatLens app
  (CatLens f) . (CatLens g) = CatLens (uncurry (curry g . curry f))


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
  => CatLens (Kleisli backend) f a b

get :: Monad m => CatLens (Kleisli m) (Const a) b a -> b -> m a
get (CatLens c) = fmap (fmap getConst) . runKleisli . curry c $ Kleisli (pure . Const)

modify :: Monad m => CatLens (Kleisli m) Identity b a -> (a -> m a) -> b -> m b
modify (CatLens c) f = fmap (fmap runIdentity) . runKleisli . curry c $ Kleisli (fmap Identity . f)

modify' :: Monad m => CatLens (Kleisli m) Identity b a -> (a -> a) -> b -> m b
modify' c f = modify c (pure . f)

user :: PersistLens IO (ID User) User
user = CatLens $ Kleisli $ \(kuu, uid) -> do
  u  <- loadUser' uid
  fu <- runKleisli kuu u
  tu <- Traversable.mapM (updateUser uid) fu
  pure $ (\_ -> uid) <$> tu

lh :: Monad backend => Lens' a b -> PersistLens backend a b
lh l = CatLens $ Kleisli $ \(kbb, a) -> do
  fb <- runKleisli kbb (a ^. l)
  pure $ (\b -> set l b a) <$> fb

test :: PersistLens IO (ID User) String
test = lh firstName . user
