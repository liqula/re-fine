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

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Refine.Backend.App.Comment where

import Refine.Backend.Prelude

import Control.Lens ((^.))

import Refine.Common.Types.Comment
import Refine.Common.Types.Core
import Refine.Prelude ((<@>))

import Refine.Backend.App.Core
import Refine.Backend.App.VDoc (validateCreateChunkRange)
import Refine.Backend.Database.Class as DB


addNote :: ID Edit -> CreateNote -> App Note
addNote pid note = do
  appLog "addNote"
  validateCreateChunkRange pid (note ^. createNoteRange)
  db $ DB.createNote pid note

addQuestion :: ID Edit -> CreateQuestion -> App CompositeQuestion
addQuestion pid question = do
  appLog "addQuestion"
  validateCreateChunkRange pid (question ^. createQuestionRange)
  CompositeQuestion <$> db (DB.createQuestion pid question) <@> []

addAnswer :: ID Question -> CreateAnswer -> App Answer
addAnswer qid answer = do
  appLog "addAnswer"
  db $ DB.createAnswer qid answer

addDiscussion :: ID Edit -> CreateDiscussion -> App Discussion
addDiscussion pid discussion = do
  appLog "addDiscussion"
  validateCreateChunkRange pid (discussion ^. createDiscussionRange)
  db $ do
    dscn <- DB.createDiscussion pid discussion
    DB.getDiscussion (dscn ^. discussionID)

addStatement :: ID Statement -> CreateStatement -> App Discussion
addStatement sid statement = do
  appLog "addStatement"
  db $ do
    _ <- DB.createStatement sid statement
    DB.getDiscussion =<< DB.discussionOfStatement sid
