{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | this is a test of the idea that the global store should consist of a client state that has no
-- (or at least little) redundancy with the server state, and and a server cache, which stores data
-- from the server for faster access.  then the render function of a react component should be
-- allowed to issue ajax requests for data missing from the cache, and render the data that's
-- already available leaving holes.  for example, a list could be rendered empty, and then the list
-- elements would be injected as they arrive from the server.
import Prelude
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Applicative.Free hiding (Pure)


-------------- free Monad from an Applicative ---- I have not found this on hackagedb

data Free f a = Pure (f a) | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f = \case
    Pure x -> Pure $ fmap f x
    Free x -> Free $ fmap (fmap f) x

instance Applicative f => Applicative (Free f) where
  pure = Pure . pure
  Pure f <*> Pure x = Pure $ f <*> x
  Free f <*> Pure x = Free $ (<*>) <$> f <*> (Pure . pure <$> x)
  Pure f <*> Free x = Free $ (<*>) <$> (Pure . pure <$> f) <*> x
  Free f <*> Free x = Free $ (<*>) <$> f <*> x

instance Applicative f => Monad (Free f) where
  Pure x >>= g = Free $ g <$> x
  Free x >>= g = Free $ (>>= g) <$> x

liftF :: f a -> Free f a
liftF = Pure

foldFree :: Monad m => (forall x. f x -> m x) -> Free f a -> m a
foldFree f = \case
  Pure x -> f x
  Free x -> f x >>= foldFree f

---------------------- equality for functors

class EqF f where
  isSame :: f a -> f b -> Same a b

data Same a b where
  Same :: Same a a
  NotSame :: Same a b

--------------------- Query and Response existential types

data Query f where
  Query :: f a -> Query f

data Response f where
  Response :: f a -> a -> Response f

extractResponse :: EqF f => Response f -> f x -> Maybe x
extractResponse (Response a r) a' = case isSame a a' of
  Same -> Just r
  _ -> Nothing

--------------------- batch query processing

type FreeAp f = Free (Ap f)

liftFreeAp :: f a -> FreeAp f a
liftFreeAp = liftF . liftAp

batchEvaluate :: forall f m a . (Monad m, EqF f) => ([Query f] -> m [Response f]) -> FreeAp f a -> m a
batchEvaluate f = foldFree g
  where
    g :: Ap f x -> m x
    g m = do
      rs <- case runAp_ ((:[]) . Query) m of
        [] -> return []
        qs -> f qs
      return $ runIdentity $ runAp (\x -> Identity . head . catMaybes $ map (`extractResponse` x) rs) m

------------------------------- example usage

-- AJAX interactions
data AJAXInteraction a where
  GetUser :: String         -> AJAXInteraction Int  -- get user id
  Login :: String -> String -> AJAXInteraction ()   -- login as a user

instance EqF AJAXInteraction where
    isSame (Login a b) (Login a' b') | (a, b) == (a', b') = Same
    isSame (GetUser a) (GetUser a') | a == a' = Same
    isSame _ _ = NotSame

type AJAX a = FreeAp AJAXInteraction a

getUser :: String -> AJAX Int
getUser = liftFreeAp . GetUser

instance Show (Query AJAXInteraction) where
  show (Query x) = case x of
    Login a b -> "Login " ++ show a ++ " " ++ show b
    GetUser u -> "GetUser " ++ show u

instance Show (Response AJAXInteraction) where
  show (Response a b) = show (Query a) -- FIXME: show b too

instance Read (Response AJAXInteraction) where
  readsPrec _ (ff "Login" -> Just (gg -> Just (a, gg -> Just (b, gg -> Just (r, s))))) = [(Response (Login a b) r, s)]
  readsPrec _ (ff "GetUser" -> Just (gg -> Just (a, gg -> Just (r, s)))) = [(Response (GetUser a) r, s)]
  readsPrec _ _ = []

ff s (dropWhile (==' ') -> s') | take (length s) s' == s = Just $ drop (length s) s'
ff _ _ = Nothing

gg x = listToMaybe $ reads x


doAJAX :: [Query AJAXInteraction] -> IO [Response AJAXInteraction]
doAJAX qs = do
  print qs
  readLn

main = batchEvaluate doAJAX $ do
  va <- getUser "a"
  vb <- getUser "b"
  vc <- getUser "c"
  if vc > 3
    then do
      vd <- getUser "d"
      return $ va + vb + vd
    else return $ va + vb
