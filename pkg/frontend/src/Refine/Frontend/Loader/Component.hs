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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Loader.Component (vdocLoader_, VDocLoaderProps(..)) where

import Refine.Frontend.Prelude

import           Web.HttpApiData (toUrlPiece)

import           Refine.Common.Types
import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Store.Types as RS
import           Refine.Prelude ()
import           Refine.Prelude.TH (makeRefineType)
import qualified Refine.Prelude.BuildInfo as BuildInfo


newtype VDocLoaderProps = VDocLoaderProps (Maybe [ID VDoc])
  deriving (Eq, Show, Generic)

makeRefineType ''VDocLoaderProps


vdocLoader :: HasCallStack => View '[VDocLoaderProps]
vdocLoader = mkView "VDocLoader" $ \props -> do
  h1_ "Load a VDoc"
  br_ []
  button_ [ "id" $= "add-vdoc-to-backend"
          , onClick $ \_ _ -> simpleHandler $ RS.dispatch RS.AddDemoDocument
          ] $
          elemString "Load generated document via backend"
  br_ []
  vdocListLoader_ props
  br_ []

  div_ $ do
    br_ [] >> br_ [] >> br_ [] >> hr_ []
    pre_ $ do
      elemString $ "commit hash: " <> show BuildInfo.gitCommitHash
      "\n"
      elemString $ "build timestamp: " <> show BuildInfo.gitBuildTimestamp
      "\n"

vdocLoader_ :: HasCallStack => VDocLoaderProps -> ReactElementM eventHandler ()
vdocLoader_  = view_ vdocLoader "vdocLoader_"

vdocListLoader :: HasCallStack => View '[VDocLoaderProps]
vdocListLoader = mkView "VDocListLoader" $ \case
  VDocLoaderProps Nothing     -> mempty
  VDocLoaderProps (Just list) -> div_ $ toButton `mapM_` list

toButton :: HasCallStack => ID VDoc -> ReactElementM 'EventHandlerCode ()
toButton li = button_
  [ "id" $= cs ("load-vdoc-list" <> show (_unID li))
  , onClick $ \_ _ -> simpleHandler . RS.dispatch . RS.LoadDocument $ li
  ]
  (elemText $ toUrlPiece li)

vdocListLoader_ :: HasCallStack => VDocLoaderProps -> ReactElementM eventHandler ()
vdocListLoader_ = view_ vdocListLoader "vdocListLoader_"
