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

module Refine.Common.VDoc.HTML.Enhance where

import           Data.Maybe (fromMaybe, isNothing)
import           Data.String (fromString)
import           Data.Text (Text)
import           Data.Tree
import           Text.HTML.Parser


addDataUidsToForest :: Forest Token -> Forest Token
addDataUidsToForest = addDataUidsToForest_ ""

addDataUidsToForest_ :: Text -> Forest Token -> Forest Token
addDataUidsToForest_ uid forest = map (addDataUidsToTree uid) forest

addDataUidsToTree :: Text -> Tree Token -> Tree Token
addDataUidsToTree uid (Node (TagOpen tagname attrs) children) =
  let attrUid = findAttrUidIn attrs
      newUid = fromMaybe uid attrUid
      newAttrs = if isNothing attrUid then ((Attr (fromString "data-uid") uid):attrs) else attrs
  in Node (TagOpen tagname newAttrs) (addDataUidsToForest_ newUid children)
  where
    findAttrUidIn :: [Attr] -> Maybe Text
    findAttrUidIn [] = Nothing
    findAttrUidIn ((Attr "data-uid" value):_) = Just value
    findAttrUidIn (_:as) = findAttrUidIn as
addDataUidsToTree uid (Node t children) = (Node t (addDataUidsToForest_ uid children))

