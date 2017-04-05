{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Views.Types where

import           Control.Lens (makeLenses)

import           Refine.Common.Types (ContributionID, CompositeDiscussion, Note, Edit)
import           Refine.Frontend.Contribution.Types (MarkPositions, QuickCreateShowState)
import           Refine.Frontend.Screen.Types (ScreenState)
import           Refine.Frontend.Types (Selection)


data AsideProps = AsideProps
  { _asideMarkPositions     :: MarkPositions
  , _asideCurrentSelection  :: Maybe Selection
  , _asideHighlightedBubble :: Maybe ContributionID
  , _asideScreenState       :: ScreenState
  , _asideDiscussions       :: [CompositeDiscussion]
  , _asideNotes             :: [Note]
  , _asideEdits             :: [Edit]
  , _asideQuickCreateShow   :: QuickCreateShowState
  }
  deriving (Eq)

makeLenses ''AsideProps
