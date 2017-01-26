{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Refine.Frontend.Loader.Component where

import           Data.Monoid ((<>))
import           Data.String (fromString)
import           React.Flux
import           Refine.Common.Types
import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Types as RS


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
