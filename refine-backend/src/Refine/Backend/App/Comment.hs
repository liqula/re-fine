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

import Refine.Common.Types.Comment
import Refine.Common.Types.Prelude
import Refine.Common.Types.VDoc
import Refine.Prelude ((<@>))

import Refine.Backend.App.Core
import Refine.Backend.Database.Core (DB)
import Refine.Backend.Database.Class as DB


addNote :: ID Patch -> Create Note -> App DB Note
addNote pid note = do
  appLog "addNote"
  db $ DB.createNote pid note

addQuestion :: ID Patch -> Create Question -> App DB CompositeQuestion
addQuestion pid question = do
  appLog "addQuestion"
  CompositeQuestion <$> db (DB.createQuestion pid question) <@> []

addAnswer :: ID Question -> Create Answer -> App DB Answer
addAnswer qid answer = do
  appLog "addAnswer"
  db $ DB.createAnswer qid answer

addDiscussion :: ID Patch -> Create Discussion -> App DB CompositeDiscussion
addDiscussion pid discussion = do
  appLog "addDiscussion"
  CompositeDiscussion <$> db (DB.createDiscussion pid discussion) <@> []

addStatement :: ID Discussion -> Create Statement -> App DB Statement
addStatement did statement = do
  appLog "addStatement"
  db $ DB.createStatement did statement

addReplyStatement :: ID Statement -> Create Statement -> App DB Statement
addReplyStatement sid statement = do
  appLog "addReplyStatement"
  db $ DB.createReplyStatement sid statement
