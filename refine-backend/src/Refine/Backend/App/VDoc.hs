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

module Refine.Backend.App.VDoc where

import Control.Lens ((^.), to, view)
import Control.Monad ((<=<), join, mapM)
import Control.Monad.Except (throwError)
import Data.Monoid ((<>))

import           Refine.Backend.App.Core
import           Refine.Backend.Database (DB)
import qualified Refine.Backend.Database.Class as DB
import qualified Refine.Backend.DocRepo as DocRepo
import           Refine.Common.Rest (CompositeVDoc(..))
import           Refine.Common.Types.Note
import           Refine.Common.Types.Prelude
import           Refine.Common.Types.VDoc
import           Refine.Common.VDoc.HTML
import           Refine.Prelude (clearTP)


listVDocs :: App DB [VDoc]
listVDocs = do
  appLog "listVDocs"
  db $ mapM DB.getVDoc =<< DB.listVDocs

createVDocGetComposite :: Create VDoc -> App DB CompositeVDoc
createVDocGetComposite = (getCompositeVDoc . view vdocID) <=< createVDoc

createVDoc :: Create VDoc -> App DB VDoc
createVDoc pv = do
  appLog "createVDoc"
  (dr, dp) <- docRepo $ do
    dr <- DocRepo.createRepo
    dp <- DocRepo.createInitialPatch dr (pv ^. createVDocInitVersion)
    pure (dr, dp)
  db $ do
    p <- DB.createPatch dp
    r <- DB.createRepo dr (p ^. patchID)
    DB.createVDoc pv r

getVDoc :: ID VDoc -> App DB VDoc
getVDoc i = do
  appLog "getVDoc"
  db $ DB.getVDoc i

getCompositeVDoc :: ID VDoc -> App DB CompositeVDoc
getCompositeVDoc vid = do
  appLog "getCompositeVDoc"
  join . db $ do
    vdoc     <- DB.getVDoc vid
    rid      <- DB.vdocRepo vid
    rhandle  <- DB.getRepoHandle rid
    headid   <- view vdocHeadPatch <$> DB.getRepo rid
    hhandle  <- DB.getPatchHandle headid
    comments <- mapM DB.getComment =<< DB.patchComments headid
    notes    <- mapM DB.getNote    =<< DB.patchNotes    headid

    let chunkRangesCN =
          (view (commentRange . to clearTP) <$> comments) <>
          (view (noteRange . to clearTP) <$> notes)

    pure $ do
      hpatches <- docRepo $ DocRepo.getChildPatches rhandle hhandle
      patches  <- db $ mapM DB.getPatchFromHandle hpatches
      let chunkRanges = chunkRangesCN <> (view (patchRange . to clearTP) <$> patches)
      version <- either (throwError . AppVDocError) pure
                 =<< insertMarks chunkRanges <$> docRepo (DocRepo.getVersion rhandle hhandle)
      pure $ CompositeVDoc vdoc version patches comments notes
