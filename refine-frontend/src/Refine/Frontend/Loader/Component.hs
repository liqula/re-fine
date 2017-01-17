{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Refine.Frontend.Loader.Component where

import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Data.String.Conversions
import           React.Flux
import           Refine.Common.VDoc
import qualified Refine.Frontend.RefineStore as RS


vdocLoader :: ReactView (Maybe [VDocListItem])
vdocLoader = defineView "VDocLoader" $ \list -> do
    h1_ "Load a VDoc"
    button_ [ "id" $= "load-demo"
            , onClick $ \_ _ -> RS.dispatch (RS.OpenDocument sampleVDoc)
            ] $
            elemString "Load dummy document"
    button_ [ "id" $= "add-vdoc-to-backend"
            , onClick $ \_ _ -> RS.dispatch RS.AddDemoDocument
            ] $
            elemString "Load generated document via backend"
    vdocListLoader_ list

vdocLoader_ :: Maybe [VDocListItem] -> ReactElementM eventHandler ()
vdocLoader_ list = view vdocLoader list mempty

vdocListLoader :: ReactView (Maybe [VDocListItem])
vdocListLoader = defineView "VDocListLoader" $ \case
        Nothing -> button_ [ "id" $= "load-vdoc-list-from-server"
                            , onClick $ \_ _ -> RS.dispatch RS.LoadDocumentList
                            ] $
                            elemString "Load list of documents from server"
        Just list -> div_ . mconcat $ map toButton list

toButton :: VDocListItem -> ReactElementM [SomeStoreAction] ()
toButton li = button_ [ "id" $= fromString ("load-vdoc-list" <> show ((_unAUID . _vdliVDoc) li))
                        , onClick $ \_ _ -> RS.dispatch . RS.LoadDocument . _vdliVDoc $ li
                        ] (elemString (cs (_vdliVDocTitle li)))


vdocListLoader_ :: Maybe [VDocListItem] -> ReactElementM eventHandler ()
vdocListLoader_ list = view vdocListLoader list mempty
