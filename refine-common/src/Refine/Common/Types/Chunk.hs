{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.Types.Chunk where

import Data.String.Conversions (ST)
import GHC.Generics (Generic)
import Refine.Prelude.TH



data ChunkPoint = ChunkPoint
  { _chunkPointNode :: Maybe DataUID
  , _chunkPointOffset :: Int
  }
  deriving (Eq, Ord, Show, Read, Generic)

-- | Location of a 'Patch', 'Comment', ... in the HTML tree of a 'VDocVersion'.  'Patch' etc. are
-- called the *owner* of the 'ChunkRange'.
--
-- ASSUMPTION: @0 <= begin < end < length of 'VDocVersion'@.
--
-- The 'ChunkRange' consists of a label and a beginning and end point.  Label is the @AUID@ of the
-- owner (or some representation thereof); beginning and end points correspond to a range object
-- provided by the browser.
data ChunkRange = ChunkRange
  { _chunkRangeLabel :: ST
  , _chunkRangeBegin :: ChunkPoint
  , _chunkRangeEnd   :: ChunkPoint
  }
  deriving (Eq, Ord, Show, Read, Generic)

newtype DataUID = DataUID { unDataUID :: Int }
  deriving (Eq, Ord, Show, Read, Generic)

-- * refine types

makeRefineType ''ChunkPoint
makeRefineType ''ChunkRange
makeRefineType ''DataUID
