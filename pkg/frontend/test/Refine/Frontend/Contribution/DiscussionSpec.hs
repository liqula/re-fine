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

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-imports -fno-warn-unused-binds #-}  -- TODO

module Refine.Frontend.Contribution.DiscussionSpec where

import Refine.Frontend.Prelude

import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Tree
import           Test.Hspec

import           React.Flux.Missing
import           Refine.Common.Test.Samples (sampleRawContent1)
import           Refine.Common.Types
import           Refine.Common.VDoc.OT
import           Refine.Common.VDoc.Draft (addMarksToRawContent)
import           Refine.Frontend.Contribution.Dialog
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Contribution.Discussion
import           Refine.Frontend.Test.Enzyme

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

testDiscussionProps :: DiscussionProps
testDiscussionProps = discussionProps disc (addMarksToRawContent [(MarkContribution (ContribIDDiscussion (ID 0)) 0, rnge)] sampleRawContent1)
  where
    rnge = Range (Position (BlockIndex 0 (BlockKey "2vutk")) 1)
                 (Position (BlockIndex 1 (BlockKey "5n4ph")) 3)

    disc = Discussion
      { _discussionMetaID = MetaID (ID 0) metaInfo1
      , _discussionPublic = True
      , _discussionRange  = rnge
      , _discussionTree   = Node statement1 [Node statement2 []]
      }

    statement1 = Statement
      { _statementMetaID = MetaID (ID 1) metaInfo1
      , _statementText   = "first statement"
      , _statementParent = Nothing
      }

    statement2 = Statement
      { _statementMetaID = MetaID (ID 2) metaInfo1
      , _statementText   = "second statement"
      , _statementParent = Just (ID 1)
      }

    metaInfo1 = MetaInfo
      { _metaCreatedBy = u1
      , _metaCreatedAt = t1
      , _metaChangedBy = u1
      , _metaChangedAt = t1
      }

    u1 = Anonymous
    t1 = Timestamp $ parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" "2017-04-06 08:44:40 CEST"

spec :: Spec
spec = do
  describe "The discussion_ component" $ do
    describe "cropToBlocks" $ do
      let rc = sampleRawContent1
          interestingRanges = [Range (Position i 0) (Position j 0) | i <- vbs, j <- vbs, i <= j]
            where
              vbs = take 3 ibs <> take 3 (reverse ibs)
              ibs = blockIndices rc

      it "works." $ do
        forM_ interestingRanges $ \r -> do
          let bkeys = fmap (view blockKey) . view rawContentBlocks $ cropToBlocks r rc
          NonEmpty.head bkeys `shouldBe` r ^. rangeBegin . blockIndex . blockIndexKey
          NonEmpty.last bkeys `shouldBe` r ^. rangeEnd . blockIndex . blockIndexKey

    describe "discussionProps" $ do
      it "gives you exactly the blocks overlapped by the discussion range." $ do
        ((^. blockKey) <$> (testDiscussionProps ^. discPropsAboutText . rawContentBlocks))
          `shouldBe` (BlockKey "2vutk" :| [BlockKey "5n4ph"])

    describe "render" $ do
      it "shows blocks overlapping with range" $ do
        wrapper <- mount $ discussion_ testDiscussionProps
        htm <- cs <$> html wrapper
        htm `shouldContain` "2vutk"
        htm `shouldContain` "5n4ph"

      it "does not show blocks not overlapping with range" $ do
        wrapper <- mount $ discussion_ testDiscussionProps
        htm <- cs <$> html wrapper
        htm `shouldNotContain` "8eupo"

      it "styles selected range (html mark tag)" $ do
        let color = "rgba(0, 255, 0, 0.3)"  -- (this isn't the color from "Colors", but the hack
                                            -- from 'mkDocumentStyleMap'.)
        wrapper <- mount $ discussion_ testDiscussionProps
        htm <- cs <$> html wrapper
        htm `shouldContain` color

      it "show texts of all statements in discussion" $ do
        pending

      it "indent statement according to its depth in the tree" $ do
        pending

    describe "events -> actions" $ do
      it "upvote discussion" $ do
        pending

      it "downvote discussion" $ do
        pending

      it "back" $ do
        pending

      it "respond to statement" $ do
        pending

      it "upvote statement" $ do
        pending

      it "downvote statement" $ do
        pending
