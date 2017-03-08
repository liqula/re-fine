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
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}


module Refine.Frontend.Document.Document where

import           Control.DeepSeq (NFData)
import           Control.Lens ((^.))
import           Data.Maybe (isJust)
import           Data.String.Conversions
import qualified Data.Tree as DT
import           GHCJS.Types (JSVal)
import           React.Flux
import           React.Flux.Internal  -- (HandlerArg(..), PropertyOrHandler(..))
import           System.IO.Unsafe (unsafePerformIO)
import qualified Text.HTML.Parser as HTMLP

import           Refine.Common.Types
import           Refine.Frontend.Contribution.Mark
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.Types
import qualified Refine.Frontend.Screen.Types as SC
import qualified Refine.Frontend.Store as RS
import           Refine.Frontend.ThirdPartyViews (editor_)
import qualified Refine.Frontend.Types as RS




document :: ReactView DocumentProps
document = defineView "Document" $ \props ->
  if isJust (props ^. dpDocumentState . dsEditMode) then
    editorWrapper_ $ EditorWrapperProps (props ^. dpVDocVersion)
  else
    article_ [ "id" $= "vdocValue"
             , "className" $= "gr-20 gr-14@desktop"
             , onMouseUp  $ \_ me -> RS.dispatch $
                 RS.TriggerUpdateSelection (SC.OffsetFromDocumentTop $ mousePageY me) (props ^. dpToolbarStatus)
                   -- <-- relative to webpage | relative to viewport -> mouseClientY me
             , onTouchEnd $ \_ te -> RS.dispatch $
                 RS.TriggerUpdateSelection (SC.OffsetFromDocumentTop . touchPageY . head $ touches te)
                                           (props ^. dpToolbarStatus)
             ] $ do
      -- leftover from p'2016:
      -- div_ ["className" $= "c-vdoc-overlay"] $ do
        -- div_ ["className" $= "c-vdoc-overlay__inner"] $ do
      div_ ["className" $= "c-article-content"] $ do
        toArticleBody (props ^. dpContributionState) (_unVDocVersion (props ^. dpVDocVersion))

document_ :: DocumentProps -> ReactElementM eventHandler ()
document_ props = view document props mempty

newtype EditorWrapperProps = EditorWrapperProps
  { _ewpVDocVersion       :: VDocVersion 'HTMLWithMarks
  }

editorWrapper :: EditorState -> ReactView ()
editorWrapper editorState_ = defineStatefulView "EditorWrapper" editorState_ $ \(EditorState editorState) () ->
  article_ ["className" $= "gr-20 gr-14@desktop"] $
    editor_ [ property "editorState" editorState
            , CallbackPropertyWithSingleArgument "onChange" $  -- 'onChange' or 'on' do not match the type we need.
                \(HandlerArg evt) _ -> js_traceEditorState evt `seq` ([{- this can be empty for now -}], Just (EditorState evt))
            ] mempty

editorWrapper_ :: EditorWrapperProps -> ReactElementM eventHandler ()
editorWrapper_ (EditorWrapperProps vers) = view (editorWrapper $ createEditorState vers) () mempty


toArticleBody :: ContributionState -> DT.Forest HTMLP.Token -> ReactElementM [SomeStoreAction] ()
toArticleBody state forest = mconcat $ map (toHTML state) forest


toHTML :: ContributionState -> DT.Tree HTMLP.Token -> ReactElementM [SomeStoreAction] ()
-- br and hr need to be handled differently
toHTML _ (DT.Node (HTMLP.TagSelfClose "br" attrs) []) = br_ (toProperties attrs)
toHTML _ (DT.Node (HTMLP.TagSelfClose "hr" attrs) []) = hr_ (toProperties attrs)
-- just a node without children, containing some text:
toHTML _ (DT.Node (HTMLP.ContentText content) []) = elemText content
toHTML _ (DT.Node (HTMLP.ContentChar content) []) = elemText $ cs [content]
-- a comment - do we want to support them, given our HTML editor provides no means of entering them?
toHTML _ (DT.Node (HTMLP.Comment _) _) = mempty -- ignore comments
toHTML state (DT.Node (HTMLP.TagOpen "mark" attrs) subForest) =
    rfMark_ (MarkProps attrs (state ^. csHighlightedMarkAndBubble) (state ^. csDisplayedContributionID)) $
      toHTML state `mapM_` subForest -- (toProperties attrs)
toHTML state (DT.Node (HTMLP.TagOpen tagname attrs) subForest) =
    React.Flux.term (cs tagname) (toProperties attrs) $ toHTML state `mapM_` subForest
toHTML _ (DT.Node (HTMLP.TagSelfClose tagname attrs) []) =
    React.Flux.term (cs tagname) (toProperties attrs) mempty

-- the above cases cover all possibilities in the demo article, but we leave this here for discovery:
toHTML _ (DT.Node rootLabel []) = p_ (elemString ("root_label_wo_children " <> show rootLabel))
toHTML state (DT.Node rootLabel subForest) = do
    p_ $ do
      elemString ("root_label " <> show rootLabel)
      p_ "subforest_start"
      toHTML state `mapM_` subForest
      p_ "subforest_end"

-- alternatively: (needs `import Text.Show.Pretty`, package pretty-show.)
-- toHTML n@(DT.Node rootLabel subForest) = pre_ $ ppShow n


newtype EditorState = EditorState { _unEditorState :: JSVal }
  deriving (NFData)

createEditorState :: VDocVersion 'HTMLWithMarks -> EditorState
createEditorState (VDocVersion _) = unsafePerformIO $ do
  content <- js_CS_createFromText "bla"
  estate <- js_ES_createWithContent content

  js_traceEditorState estate `seq` pure ()

  pure $ EditorState estate


foreign import javascript unsafe
    "{eventState: $1}"
    js_eventToEditorState :: JSVal -> JSVal

foreign import javascript unsafe
    "refine$traceEditorState($1)"
    js_traceEditorState :: JSVal -> ()


-- * https://draftjs.org/docs/api-reference-editor-state.html

-- | https://draftjs.org/docs/api-reference-editor-state.html#createwithcontent
foreign import javascript unsafe
    "Draft.EditorState.createWithContent($1)"
    js_ES_createWithContent :: JSVal -> IO JSVal


-- * https://draftjs.org/docs/api-reference-content-state.html

-- | https://draftjs.org/docs/api-reference-content-state.html#createfromtext
foreign import javascript unsafe
    "Draft.ContentState.createFromText($1)"
    js_CS_createFromText :: JSString -> IO JSVal
