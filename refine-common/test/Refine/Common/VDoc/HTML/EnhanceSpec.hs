{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.VDoc.HTML.EnhanceSpec where

import           Data.Tree
import           Test.Hspec
import           Text.HTML.Parser
import Refine.Common.VDoc.HTML.Enhance

openTagWithUID :: Token
openTagWithUID = TagOpen "tag" [(Attr "data-uid" "77")]

openTagWithOtherUID :: Token
openTagWithOtherUID = TagOpen "tag" [(Attr "data-uid" "13")]

openTagWithoutUID :: Token
openTagWithoutUID = TagOpen "tag" []

spec :: Spec
spec = parallel $ do
  describe "Enhance" $ do
    describe "addDataUidsToTree" $ do

      it "adds the passed uid when there is none" $ do
        addDataUidsToTree "1" (Node openTagWithoutUID []) `shouldBe` (Node (TagOpen "tag" [(Attr "data-uid" "1")]) [])

      it "ignores the passed uid when there is already one" $ do
        addDataUidsToTree "1" (Node openTagWithUID []) `shouldBe` (Node openTagWithUID [])

      it "does not alter other tags" $ do
        addDataUidsToTree "1" (Node (TagClose "tag")     []) `shouldBe` (Node (TagClose "tag") [])
        addDataUidsToTree "1" (Node (ContentText "text") []) `shouldBe` (Node (ContentText "text") [])
        addDataUidsToTree "1" (Node (ContentChar 'x')    []) `shouldBe` (Node (ContentChar 'x') [])
        addDataUidsToTree "1" (Node (Comment "secret")   []) `shouldBe` (Node (Comment "secret") [])
        addDataUidsToTree "1" (Node (Doctype "type")     []) `shouldBe` (Node (Doctype "type") [])

      it "passes the present uid to all children that do not already have one" $ do
        addDataUidsToTree "1" (Node openTagWithUID [(Node openTagWithoutUID []), (Node openTagWithOtherUID [])])
          `shouldBe` (Node openTagWithUID [(Node (TagOpen "tag" [(Attr "data-uid" "77")]) []), (Node openTagWithOtherUID [])])

      it "passes the passed uid to all children that do not already have one if none is present" $ do
        addDataUidsToTree "1" (Node openTagWithoutUID [(Node openTagWithoutUID []), (Node openTagWithOtherUID [])])
          `shouldBe` (Node (TagOpen "tag" [(Attr "data-uid" "1")]) [(Node (TagOpen "tag" [(Attr "data-uid" "1")]) []), (Node openTagWithOtherUID [])])

