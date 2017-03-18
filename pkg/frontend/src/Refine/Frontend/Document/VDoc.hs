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


module Refine.Frontend.Document.VDoc where

import           Control.Lens ((^.))
import           Data.JSString (JSString)
import           Data.String.Conversions
import           Data.Tree
import           React.Flux
import           React.Flux.Internal (PropertyOrHandler(..))
import           Text.HTML.Parser
import           Web.HttpApiData

import           Refine.Common.Types
import           Refine.Frontend.Contribution.Mark
import           Refine.Frontend.Contribution.Types
import qualified Refine.Frontend.Test.Console as Console


vdocToHTML :: ContributionState -> VDocVersion 'HTMLWithMarks -> ReactElementM [SomeStoreAction] ()
vdocToHTML state (VDocVersion forest) = forestToHTML state forest

forestToHTML :: ContributionState -> Forest Token -> ReactElementM [SomeStoreAction] ()
forestToHTML state = mapM_ (treeToHTML state)

treeToHTML :: ContributionState -> Tree Token -> ReactElementM [SomeStoreAction] ()
treeToHTML state = go
  where
    go tree@(Node (TagOpen tagname attrs) children) = tag $ forestToHTML state children
      where
        props cid = MarkProps
          (toProperty <$> attrs)
          cid
          (state ^. csHighlightedMarkAndBubble)
          (state ^. csDisplayedContributionID)

        tag = if tagname == "mark"
          then case contributionIdFrom attrs of
            Just cid -> rfMark_ (props cid)
            Nothing  -> warn "mark tag without data-contribution-id" tree `seq` mempty
          else React.Flux.term (cs tagname) (toProperty <$> attrs)

    go (Node (TagSelfClose tagname attrs) []) =
      React.Flux.term (cs tagname) (toProperty <$> attrs) mempty

    go (Node (ContentText content) []) = elemText content
    go (Node (ContentChar content) []) = elemText $ cs [content]

    go (Node (Comment _) [])     = mempty
    go bad@(Node (Doctype _) []) = warn "unexpected Doctype token" bad `seq` mempty
    go bad@(Node (TagClose _) _) = warn "illegal TagClose token"   bad `seq` mempty
    go bad@(Node _ (_:_))        = warn "flat node with children"  bad `seq` mempty

    warn :: JSString -> Tree Token -> ()
    warn msg = Console.consoleLogJSON ("WARNING: treeToHTML: " <> msg)
      -- TODO: make this a gracefulError; consolidate gracefulError with Test.Console module.


toProperty :: Attr -> forall handler. PropertyOrHandler handler
toProperty (Attr key value) = cs key $= cs value

-- FUTUREWORK: this should not be 'Maybe', but we don't know what to put here if it is.
contributionIdFrom :: [Attr] -> Maybe ContributionID
contributionIdFrom attrs =
  either (const Nothing) Just . parseUrlPiece . cs =<< attribValueOf "data-contribution-id" attrs

attribValueOf :: ST -> [Attr] -> Maybe ST
attribValueOf _ [] = Nothing
attribValueOf wantedKey (Attr key value : _) | key == wantedKey = Just value
attribValueOf wantedKey (_ : as) = attribValueOf wantedKey as
