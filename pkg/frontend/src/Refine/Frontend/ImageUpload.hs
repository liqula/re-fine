{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.ImageUpload where
#include "import_frontend.hs"

import           Language.Css.Syntax
import           System.IO.Unsafe (unsafePerformIO)

import           React.Flux.Missing
import           Refine.Common.Types
import           Refine.Common.VDoc.Draft
import           Refine.Frontend.Access
import           Refine.Frontend.Icon
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Util
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Types


-- TODO:img
-- replace 'editable' boolean with access policy guard
-- generalise away from user to arbitrary image
-- generalise over action
-- default image


imageUpload_ :: Bool -> User -> LocalStateRef ProfileLocalState -> ProfileLocalState
             -> ReactElementM_ (React.EventHandlerType ('React.StatefulEventHandlerCode ProfileLocalState)) ()
imageUpload_ editable user lst st = do
    elemText $ user ^. userName

    hr_ []
    case user ^. userAvatar of
      Nothing -> elemText "You didn't upload an avatar yet."
      Just (ImageInline source) ->
        img_ [ "src" $= cs source
             , "style" @@= [ decl "maxWidth" (Px 200)
                           , decl "maxHeight" (Px 200)
                           ]
             ] $ pure ()

    when editable $ do
      input_ [ "type" $= "file"
             , onChange $ \evt -> simpleHandler $ \st' -> case unsafePerformIO . fromJSVal $ target evt "files" of
                 Just [f] ->
                   ( [action @GlobalState . MainMenuAction . MainMenuActionOpen $ MainMenuProfile (user ^. userID, FormBegin lst)]
                   , Just $ st' & _1 .~ Just (Left $ NoJSONRep f))
                 _ -> ([], Nothing)
             ]

      case fst st of
        Just (Right (ImageInline source)) -> do
          br_ []
          img_ [ "src" $= cs source
               , "style" @@= [ decl "maxWidth" (Px 200)
                             , decl "maxHeight" (Px 200)
                             ]
               ] $ pure ()
          button_
            [ onClick $ \_evt _ -> simpleHandler $
              \st' -> ( [action @GlobalState . MainMenuAction . MainMenuActionOpen $
                          MainMenuProfile ( user ^. userID
                                          , FormComplete (fst st, Just $ user ^. userDescription))]
                      , Just $ st' & _1 .~ Nothing)
            ] $ elemText "upload"
        _ -> pure ()
