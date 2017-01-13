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

module Refine.Backend.Server
  ( startBackend
  , refineApi
  ) where

import           Control.Category
import           Control.Monad.Except
import qualified Control.Natural as CN
import           Control.Natural (($$))
import           Data.String.Conversions (cs)
import           Network.Wai.Handler.Warp as Warp
import           Prelude hiding ((.), id)
import           Servant hiding (Patch)

import Refine.Backend.App
import Refine.Backend.App.MigrateDB
import Refine.Backend.Database (DB, DBConfig(..), createDBRunner)
import Refine.Backend.Logger
import Refine.Backend.Natural
import Refine.Backend.DocRepo (createRunRepo)
import Refine.Common.Rest
import Refine.Common.Types


startBackend :: IO ()
startBackend = do
  runDb      <- createDBRunner $ DBOnDisk "refine.db"
  runDocRepo <- createRunRepo "."
  let logger = Logger putStrLn
      app    = runApp runDb runDocRepo logger

  void $ (natThrowError . app) $$ do
    migrateDB

  Warp.runSettings Warp.defaultSettings
    . Servant.serve (Proxy :: Proxy RefineAPI)
    $ serverT app refineApi


serverT :: (App db CN.:~> ExceptT AppError IO) -> ServerT RefineAPI (App db) -> Server RefineAPI
serverT app = enter (toServantError . cnToSn app)


toServantError :: (Monad m) => ExceptT AppError m :~> ExceptT ServantErr m
toServantError = Nat ((lift . runExceptT) >=> either (throwError . fromAppError) pure)
  where
    -- FIXME: Render JSON from the errors
    fromAppError :: AppError -> ServantErr
    fromAppError (AppUnknownError msg)      = err500 { errBody = cs msg }
    fromAppError (AppDBError      dbError)  = err500 { errBody = cs $ show dbError }
    fromAppError (AppDocRepoError docError) = err500 { errBody = cs $ show docError }


-- | The 's' prefix in the handlers stands for "server", and is used to dismabiguate between the code in
-- 'App' vs. the code in 'ServerT'.  This is slightly less noisy than qualified imports, and it's
-- (probably) only used internally in this module.
refineApi :: ServerT RefineAPI (App DB)
refineApi =
       sGetVDocs
  :<|> sGetVDoc
  :<|> sPostVDoc
  :<|> sPutTitle
  :<|> sPutAbstract
  :<|> sDeleteVDoc
  :<|> sPostComment
  :<|> sDeleteComment
  :<|> sPostNote
  :<|> sDeleteNote
  :<|> sPutNoteVisibility
  :<|> sGetVersion
  :<|> sPostPatch
  :<|> sDeletePatch
  :<|> sMergePatch
  :<|> sRebasePatch
  :<|> sPutVote
  :<|> sDeleteVote


-- * vdocs

sGetVDocs :: App DB [ID VDoc]
sGetVDocs = undefined

sGetVDoc :: ID VDoc -> App DB VDoc
sGetVDoc = undefined

sPostVDoc :: Proto VDoc -> App DB (ID VDoc)
sPostVDoc = undefined

sPutTitle :: ID VDoc -> Title -> App DB ()
sPutTitle = undefined

sPutAbstract :: ID VDoc -> Abstract -> App DB ()
sPutAbstract = undefined

sDeleteVDoc :: ID VDoc -> App DB VDoc
sDeleteVDoc = undefined


-- * left ui column (coments, notes, ...)

sPostComment :: ID Patch -> Proto Comment -> App DB (ID Comment)
sPostComment = undefined

sDeleteComment :: ID Comment -> App DB Comment
sDeleteComment = undefined

sPostNote :: ID Patch -> Proto Note -> App DB (ID Note)
sPostNote = undefined

sDeleteNote :: ID Note -> App DB Note
sDeleteNote = undefined

sPutNoteVisibility :: ID Note -> Maybe Bool -> App DB ()
sPutNoteVisibility = undefined


-- * right ui column (patches)

sGetVersion :: ID Patch -> App DB (VDocVersion 'HTMLWithMarks)
sGetVersion = undefined

sPostPatch :: ID Patch -> Proto Patch -> App DB (ID Patch)
sPostPatch = undefined

sDeletePatch :: ID Patch -> App DB Patch
sDeletePatch = undefined

sMergePatch :: ID Patch -> App DB (ID Patch)
sMergePatch = undefined

sRebasePatch :: ConflictResolution -> App DB (ID Patch)
sRebasePatch = undefined


-- * votes

sPutVote :: ID Patch -> Proto Vote -> App DB (ID Vote)
sPutVote = undefined

sDeleteVote :: ID Vote -> App DB Vote
sDeleteVote = undefined
