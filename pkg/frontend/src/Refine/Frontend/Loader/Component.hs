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

module Refine.Frontend.Loader.Component where

import           Data.Monoid ((<>))
import           Data.String (fromString)
import           React.Flux
import           Refine.Common.Types
import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Types as RS
import qualified Refine.Prelude.BuildInfo as BuildInfo


vdocLoader :: ReactView (Maybe [ID VDoc])
vdocLoader = defineView "VDocLoader" $ \list -> do
  h1_ "Load a VDoc"
{-
  button_ [ "id" $= "load-demo"
          , onClick $ \_ _ -> RS.dispatch (RS.OpenDocument sampleVDoc)
          ] $
          elemString "Load dummy document"
-}
  button_ [ "id" $= "add-vdoc-to-backend"
          , onClick $ \_ _ -> RS.dispatch RS.AddDemoDocument
          ] $
          elemString "Load generated document via backend"
  vdocListLoader_ list

  div_ $ do
    br_ [] >> br_ [] >> br_ [] >> hr_ []
    pre_ $ do
      elemString $ "commit hash: " <> show BuildInfo.gitCommitHash
      "\n"
      elemString $ "build timestamp: " <> show BuildInfo.gitBuildTimestamp
      "\n"

vdocLoader_ :: Maybe [ID VDoc] -> ReactElementM eventHandler ()
vdocLoader_ list = view vdocLoader list mempty

vdocListLoader :: ReactView (Maybe [ID VDoc])
vdocListLoader = defineView "VDocListLoader" $ \case
  Nothing -> button_ [ "id" $= "load-vdoc-list-from-server"
                      , onClick $ \_ _ -> RS.dispatch RS.LoadDocumentList
                      ] $
                      elemString "Load list of documents from server"
  Just list -> div_ . mconcat $ map toButton list

toButton :: ID VDoc -> ReactElementM [SomeStoreAction] ()
toButton li = button_ [ "id" $= fromString ("load-vdoc-list" <> show (_unID li))
                      , onClick $ \_ _ -> RS.dispatch . RS.LoadDocument $ li
                      ] $ elemString "A document on the server"


vdocListLoader_ :: Maybe [ID VDoc] -> ReactElementM eventHandler ()
vdocListLoader_ list = view vdocListLoader list mempty
