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

module Refine.Backend.App.Comment where

import Control.Lens ((^.))

import Refine.Common.Types.Comment
import Refine.Common.Types.Prelude
import Refine.Common.Types.VDoc
import Refine.Prelude ((<@>))

import Refine.Backend.App.Core
import Refine.Backend.App.VDoc (validateCreateChunkRange)
import Refine.Backend.Database.Core (DB)
import Refine.Backend.Database.Class as DB


addNote :: ID Edit -> Create Note -> App DB Note
addNote pid note = do
  appLog "addNote"
  validateCreateChunkRange pid (note ^. createNoteRange)
  db $ DB.createNote pid note

addQuestion :: ID Edit -> Create Question -> App DB CompositeQuestion
addQuestion pid question = do
  appLog "addQuestion"
  validateCreateChunkRange pid (question ^. createQuestionRange)
  CompositeQuestion <$> db (DB.createQuestion pid question) <@> []

addAnswer :: ID Question -> Create Answer -> App DB Answer
addAnswer qid answer = do
  appLog "addAnswer"
  db $ DB.createAnswer qid answer

addDiscussion :: ID Edit -> Create Discussion -> App DB CompositeDiscussion
addDiscussion pid discussion = do
  appLog "addDiscussion"
  validateCreateChunkRange pid (discussion ^. createDiscussionRange)
  db $ do
    dscn <- DB.createDiscussion pid discussion
    DB.compositeDiscussion (dscn ^. discussionID)

addStatement :: ID Statement -> Create Statement -> App DB CompositeDiscussion
addStatement sid statement = do
  appLog "addStatement"
  db $ do
    _ <- DB.createStatement sid statement
    DB.compositeDiscussion =<< DB.discussionOfStatement sid
