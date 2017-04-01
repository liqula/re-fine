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

import           Refine.Common.Types (ContributionID, CompositeDiscussion, Note)
import           Refine.Frontend.Contribution.Types (MarkPositions, Selection)
import           Refine.Frontend.Header.Types (ToolbarExtensionStatus)
import           Refine.Frontend.Screen.Types (ScreenState)


data LeftAsideProps = LeftAsideProps
  { _leftAsideMarkPositions     :: MarkPositions
  , _leftAsideCurrentSelection  :: Selection
  , _leftAsideHighlightedBubble :: Maybe ContributionID
  , _leftAsideScreenState       :: ScreenState
  , _leftAsideDiscussions       :: [CompositeDiscussion]
  , _leftAsideNotes             :: [Note]
  , _leftAsideQuickCreateInfo   :: ToolbarExtensionStatus
  }
  deriving (Eq)

makeLenses ''LeftAsideProps
