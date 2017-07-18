{-# LANGUAGE NoImplicitPrelude          #-}
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

import Refine.Frontend.Prelude

import           Control.Lens (makeLenses)

import           Refine.Common.Types (MarkID, Discussion, Note, Edit)
import           Refine.Frontend.Contribution.Types (AllVerticalSpanBounds, BubblePositioning, QuickCreateShowState)
import           Refine.Frontend.Screen.Types (ScreenState)
import           Refine.Frontend.Types


data AsideProps = AsideProps
  { _asideAllVerticalSpanBounds :: AllVerticalSpanBounds
  , _asideMinimumSpanYPos       :: OffsetFromDocumentTop
  , _asideCurrentRange          :: Maybe SelectionStateWithPx
  , _asideHighlighteds          :: [MarkID]
  , _asideScreenState           :: ScreenState
  , _asideDiscussions           :: [Discussion]
  , _asideNotes                 :: [Note]
  , _asideEdits                 :: [Edit]
  , _asideBubblePositioning     :: BubblePositioning
  , _asideQuickCreateShow       :: QuickCreateShowState
  }
  deriving (Eq)

makeLenses ''AsideProps
