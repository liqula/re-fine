{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Prelude
  ( -- * things we need often enough
    module P

    -- * generic json
  , gtoJSONDef
  , gparseJSONDef

    -- * time
  , Timestamp(..), unTimestamp
  , timestampToEpoch
  , readTimestamp
  , showTimestamp
  , HasCurrentTime (getCurrentTimestamp)
  , timestampFormat
  , timestampFormatLength
  , Timespan(..)
  , showTimespan
  , timespanUs
  , timespanDays
  , timespanToNominalDiffTime
  , diffTimestamps
  , addTimespan
  , fromNow

    -- * misc
  , ClearTypeParameter(..)
  , leftToError
  , nothingToError
  , finally
  , justIf
  , justIfP
  , toEnumMay
  , ordNub
  , joinE
  , (<..>)
  , (<...>)
  , (<@>)
  , sortOn
  , downSortOn
  , countEq
  , iterateM
  , commonPrefix
  , Recursion(..), recursion
  ) where

import Control.Applicative as P
import Control.Arrow as P ((&&&), (***), first, second)
import Control.Category as P
import Control.Exception as P (SomeException(..), ErrorCall(ErrorCall), throwIO, try, catch, assert)
import Control.Lens as P (Iso', Prism', Lens', Getter, (&), (^.), (^?), (^?!), (.~), (%~), _1, _2, _3, at, to, has, _Just, makeLenses, makePrisms, view, set, iso, prism', lengthOf, folded, filtered)
import Control.Monad as P ((>=>), (<=<), mapM, mapM_, forM, forM_, void, foldM, join, when, unless)
import Control.Monad.Except as P (MonadError(..), ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class as P (MonadIO, liftIO)
import Control.Monad.Reader.Class as P (MonadReader, ask, asks)
import Control.Monad.State.Class as P (MonadState, state, modify, get, gets)
import Control.Monad.STM as P
import Control.Monad.Trans.Class as P
import Control.Monad.Trans.Control as P
import Control.Monad.Trans.Identity as P
import Control.Monad.Trans.State as P (StateT, runStateT)
import Control.Natural as P (($$), unwrapNT)
import Data.Char as P (isSpace, toUpper, toLower)
import Data.Coerce as P (coerce)
import Data.Data as P (Data)
import Data.Default as P (Default(def))
import Data.Either as P (either)
import Data.Function as P (on)
import Data.Functor.Infix as P ((<$$>))
import Data.List as P ((\\), foldl', sort, nub, sortBy, insertBy, replicate)
import Data.Maybe as P (catMaybes, fromMaybe, fromJust, isJust, isNothing, maybeToList, listToMaybe)
import Data.Monoid as P
import Data.Proxy as P
import Data.String as P
import Data.String.Conversions as P
import Data.Time as P
import Data.Typeable as P (Typeable, typeOf)
import Data.Void as P
import GHC.Generics as P (Generic)
import GHC.Stack as P (HasCallStack)
import Prelude as P hiding ((.), id)
import Text.Read as P (readEither, readMaybe)
import Web.HttpApiData as P (ToHttpApiData, FromHttpApiData, toUrlPiece, parseUrlPiece)
import Data.IntMap.Strict as P (IntMap)
import Data.Map.Strict as P (Map)
import Data.Set as P (Set)

import           Data.Ord
import qualified Data.Set as Set
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Refine.Prelude.Generic
import Refine.Prelude.TH

{-# ANN module "HLint: ignore Use cs" #-}


-- * time

newtype Timestamp = Timestamp { _unTimestamp :: UTCTime }
  deriving (Eq, Ord, Generic)

makeRefineType ''Timestamp

timestampToEpoch :: Timestamp -> Integer
timestampToEpoch = round . utcTimeToPOSIXSeconds . _unTimestamp

instance Show Timestamp where
    show = showTimestamp

instance Read Timestamp where
    readsPrec _ s = case splitAt timestampFormatLength $ dropWhile isSpace s of
        (readTimestamp -> Just t, r) -> [(t, r)]
        _                            -> error $ "Read Timestamp: " <> show s

readTimestamp :: String -> Maybe Timestamp
readTimestamp = fmap Timestamp . parseTimeM True defaultTimeLocale timestampFormat

showTimestamp :: Timestamp -> String
showTimestamp = formatTime defaultTimeLocale timestampFormat . _unTimestamp

class HasCurrentTime m where
  getCurrentTimestamp :: m Timestamp

instance HasCurrentTime IO where
  getCurrentTimestamp = Timestamp <$> getCurrentTime

timestampFormat :: String
timestampFormat = "%F_%T_%q"

timestampFormatLength :: Int
timestampFormatLength = length ("1864-04-13_13:01:33_846177415049" :: String)


data Timespan =
    TimespanUs    Integer
  | TimespanMs    Integer
  | TimespanSecs  Integer
  | TimespanMins  Integer
  | TimespanHours Integer
  | TimespanDays  Integer
  deriving (Eq, Ord, Show, Read, Generic)

makeRefineType ''Timespan

showTimespan :: Timespan -> String
showTimespan (TimespanUs    i) = show i <> "us"
showTimespan (TimespanMs    i) = show i <> "ms"
showTimespan (TimespanSecs  i) = show i <> "s"
showTimespan (TimespanMins  i) = show i <> "m"
showTimespan (TimespanHours i) = show i <> "h"
showTimespan (TimespanDays  i) = show i <> "d"


-- | FIXME: make this a 'Lens''
timespanUs :: Timespan -> Int
timespanUs (TimespanUs    i) = fromIntegral   i
timespanUs (TimespanMs    i) = fromIntegral $ i * 1000
timespanUs (TimespanSecs  i) = fromIntegral $ i * (1000 * 1000)
timespanUs (TimespanMins  i) = fromIntegral $ i * (1000 * 1000 * 60)
timespanUs (TimespanHours i) = fromIntegral $ i * (1000 * 1000 * 3600)
timespanUs (TimespanDays  i) = fromIntegral $ i * (1000 * 1000 * 3600 * 24)

-- | FIXME: make this a 'Lens''
timespanDays :: Timespan -> Int
timespanDays = (`div` (1000 * 1000 * 3600 * 24)) . timespanUs


timespanToNominalDiffTime :: Timespan -> NominalDiffTime
timespanToNominalDiffTime tdiff = fromRational (fromIntegral (timespanUs tdiff) / (1000 * 1000) :: Rational)

diffTimestamps :: Timestamp -> Timestamp -> Timespan
diffTimestamps (Timestamp tfrom) (Timestamp ttill) = TimespanUs .
    round $ (tfrom `diffUTCTime` ttill) * (1000 * 1000)

addTimespan :: Timespan -> Timestamp -> Timestamp
addTimespan tdiff (Timestamp tfrom) = Timestamp $
    timespanToNominalDiffTime tdiff `addUTCTime` tfrom

fromNow :: Timestamp -> Iso' Timestamp Timespan
fromNow now = iso (`diffTimestamps` now) (`addTimespan` now)


-- * misc

-- | Clear Type Param
--
-- After clearing the type parameters we could unify information
-- from different sources.
class ClearTypeParameter (t :: * -> *) where
  clearTypeParameter :: t a -> t Void


-- | Convert (Left e) to an MonadError and throw it.
leftToError :: (Monad m, MonadError me m) => (e -> me) -> Either e r -> m r
leftToError err = either (throwError . err) pure

nothingToError :: (Monad m, MonadError me m) => me -> Maybe a -> m a
nothingToError err = maybe (throwError err) pure

-- | Runs the first computation and the second one, even if
-- the first created an error.
finally :: (MonadError e m) => m a -> m () -> m a
finally m f =
  do {x <- m; f; pure x}
  `catchError`
  (\e -> do {f; throwError e})

justIf :: a -> Bool -> Maybe a
justIf x b = if b then Just x else Nothing

justIfP :: a -> (a -> Bool) -> Maybe a
justIfP x f = justIf x (f x)

toEnumMay :: forall a. (Enum a, Bounded a) => Int -> Maybe a
toEnumMay i = if i >= 0 && i <= fromEnum (maxBound :: a)
    then Just $ toEnum i
    else Nothing

ordNub :: Ord a => [a] -> [a]
ordNub = Set.toList . Set.fromList

joinE :: Either a (Either b c) -> Either (Either a b) c
joinE (Right (Right c)) = Right c
joinE (Right (Left b))  = Left (Right b)
joinE (Left a)          = Left (Left a)

infixr 9 <..>

(<..>) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(<..>) f g x y = f $ g x y

infixr 9 <...>

(<...>) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(<...>) f g x y z = f $ g x y z

infixl 4 <@>

(<@>) :: Applicative f => f (a -> b) -> a -> f b
(<@>) f x = f <*> pure x

sortOn :: Ord b => Getter a b -> [a] -> [a]
sortOn l = sortBy (compare `on` view l)

downSortOn :: Ord b => Getter a b -> [a] -> [a]
downSortOn l = sortOn (l . to Data.Ord.Down)

countEq :: (Foldable f, Eq value) => value -> Lens' vote value -> f vote -> Int
countEq v l = lengthOf $ folded . filtered ((== v) . view l)

iterateM :: (Monad m) => Int -> (a -> m a) -> a -> m a
iterateM n f x = foldM (&) x $ replicate n f

commonPrefix :: Eq a => [a] -> [a] -> ([a], [a], [a])
commonPrefix = go []
  where
    go !ps [] bs = (reverse ps, [], bs)
    go !ps as [] = (reverse ps, as, [])
    go !ps as'@(a : as) bs'@(b : bs)
      | a == b = go (a:ps) as bs
      | otherwise = (reverse ps, as', bs')

data Recursion a b c = Run a | Fail b | Halt c
  deriving (Eq, Show)

recursion :: MonadError b m => (a -> Recursion a b c) -> a -> m c
recursion f = go
  where
    go z = case f z of
      Run  y -> go y
      Fail e -> throwError e
      Halt r -> pure r


{-

-- common

import Data.Aeson as P
import Data.Aeson.Types as P
import Generics.SOP as P
import Generics.SOP.JSON as P
import Generics.SOP.NFData as P
import Refine.Prelude as P
import Servant.API as P
import Servant as P
import Servant.Utils.Enter as P
import Servant.Utils.Links as P (safeLink)


-- backend

import Database.Persist as P
import Database.Persist.Sql as P
import Database.Persist.Sqlite as P
import Database.Persist.TH as P
import Database.SQLite.Simple as P
import Database.SQLite.Simple.FromRow as P
import Data.Yaml as P
import Network.HTTP.Types as P
import Network.HTTP.Types.Status as P
import Network.URI as P
import Network.Wai as P
import Network.Wai.Handler.Warp as P
import Refine.Common.Prelude as P
import Servant.Cookie.Session as P
import Servant.Server.Internal as P
import System.Directory as P
import System.Environment as P
import System.Exit as P
import System.FilePath as P
import System.IO as P
import System.IO.Temp as P
import System.Process as P


-- frontend

import Data.JSString as P
import GHCJS.Foreign.Callback as P (Callback, asyncCallback)
import GHCJS.Marshal as P
import GHCJS.Marshal.Pure as P
import GHCJS.Types as P
import React.Flux.Addons.Servant as P
import React.Flux as P
import Refine.Common.Prelude as P

-}
