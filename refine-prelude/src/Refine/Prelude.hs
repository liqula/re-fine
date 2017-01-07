{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Refine.Prelude
  ( -- * generic json
    gtoJSONDef
  , gparseJSONDef

    -- * time
  , Timestamp(..), unTimestamp
  , timestampToEpoch
  , readTimestamp
  , showTimestamp
  , timestampFormat
  , timestampFormatLength
  , Timespan(..)
  , showTimespan
  , timespanUs
  , timespanDays
  , diffTimestamps
  , addTimespan
  , fromNow

    -- * misc
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
  ) where

import           Control.Lens
import           Control.Monad (foldM)
import           Data.Char (isSpace)
import           Data.Function (on)
import           Data.List (replicate, sortBy)
import           Data.Monoid ((<>))
import           Data.Ord
import qualified Data.Set as Set
import           Data.Time
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified GHC.Generics as GHC

import Refine.Prelude.Generic
import Refine.Prelude.TH


-- * time

newtype Timestamp = Timestamp { _unTimestamp :: UTCTime }
  deriving (Eq, Ord, GHC.Generic)

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
  deriving (Eq, Ord, Show, Read, GHC.Generic)

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


diffTimestamps :: Timestamp -> Timestamp -> Timespan
diffTimestamps (Timestamp tfrom) (Timestamp ttill) = TimespanUs .
    round $ (tfrom `diffUTCTime` ttill) * (1000 * 1000)

addTimespan :: Timespan -> Timestamp -> Timestamp
addTimespan tdiff (Timestamp tfrom) = Timestamp $
    fromRational (fromIntegral (timespanUs tdiff) / (1000 * 1000) :: Rational) `addUTCTime` tfrom

fromNow :: Timestamp -> Iso' Timestamp Timespan
fromNow now = iso (`diffTimestamps` now) (`addTimespan` now)





-- * misc

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
