{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.Rest where

import Servant.API

import Refine.Common.Types


-- | The 'S' prefix in the handlers stands for "server" (see 'refineApi' for an explanation).
type RefineAPI =
       SListVDocs
  :<|> SGetVDoc
  :<|> SCreateVDoc
  :<|> SAddEdit
  :<|> SAddNote
  :<|> SAddQuestion
  :<|> SAddAnswer
  :<|> SAddDiscussion
  :<|> SAddStatement


type SListVDocs
  = "r" :> "vdocs"
    :> Get '[JSON] [VDoc]

type SGetVDoc
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc)
    :> Get '[JSON] CompositeVDoc

type SCreateVDoc
  = "r" :> "vdoc" :> ReqBody '[JSON] (Create VDoc)
    :> Post '[JSON] CompositeVDoc

type SAddEdit
  = "r" :> "edit" :> Capture "oneditid" (ID Edit) :> ReqBody '[JSON] (Create Edit)
    :> Post '[JSON] Edit

type SAddNote
  = "r" :> "note" :> Capture "oneditid" (ID Edit) :> ReqBody '[JSON] (Create Note)
    :> Post '[JSON] Note

type SAddQuestion
  = "r" :> "question" :> Capture "oneditid" (ID Edit) :> ReqBody '[JSON] (Create Question)
    :> Post '[JSON] CompositeQuestion

type SAddAnswer
  = "r" :> "answer" :> Capture "onquestionid" (ID Question) :> ReqBody '[JSON] (Create Answer)
    :> Post '[JSON] Answer

type SAddDiscussion
  = "r" :> "discussion" :> Capture "oneditid" (ID Edit) :> ReqBody '[JSON] (Create Discussion)
    :> Post '[JSON] CompositeDiscussion

type SAddStatement
  = "r" :> "statement" :> "reply" :> Capture "onstatementid" (ID Statement) :> ReqBody '[JSON] (Create Statement)
    :> Post '[JSON] CompositeDiscussion