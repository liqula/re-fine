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
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}


module Refine.Frontend.Header.Store
( headerStateUpdate
) where

import           Control.Lens ((&), (%~))

import Refine.Frontend.Header.Types
import Refine.Frontend.Types

headerStateUpdate :: RefineAction -> HeaderState -> HeaderState
headerStateUpdate action state =
  let newState = state
                  & hsCommentToolbarExtensionIsVisible  %~ commentToolbarExtensionUpdate action
                  & hsEditToolbarExtensionIsVisible     %~ editToolbarExtensionUpdate action
                  & hsTextSpecificComment               %~ textSpecificCommentUpdate action
  in newState

---------------------------------------------------------------------------

commentToolbarExtensionUpdate :: RefineAction -> Bool -> Bool
commentToolbarExtensionUpdate action state = case action of
    HeaderAction ToggleCommentToolbarExtension -> not state
    _ -> state

editToolbarExtensionUpdate :: RefineAction -> Bool -> Bool
editToolbarExtensionUpdate action state = case action of
    HeaderAction ToggleEditToolbarExtension -> not state
    _ -> state

textSpecificCommentUpdate :: RefineAction -> TextSpecificComment -> TextSpecificComment
textSpecificCommentUpdate action state = case action of
    HeaderAction StartTextSpecificComment  -> TextSpecificCommentInProgress
    HeaderAction FinishTextSpecificComment -> TextSpecificCommentInactive
    _ -> state

