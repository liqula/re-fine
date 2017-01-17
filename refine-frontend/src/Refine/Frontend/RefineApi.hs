{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Refine.Frontend.RefineApi where

import           Data.Proxy
import           React.Flux.Addons.Servant
import           Refine.Common.Api
import           Refine.Common.VDoc


cfg :: ApiRequestConfig RestAPI
cfg = ApiRequestConfig "" NoTimeout


-- TODO: perhaps we can probably extend @RestAPIEndPoint@ with a class method that is defined in
-- terms of 'request'.

-- | obtain a list of all vdocs stored in the backend with auid, title.
listVDocs :: HandleResponse [VDocListItem] -> IO ()
listVDocs = request cfg (Proxy :: Proxy (RestAPIEndPointType ('EPVDoc 'EPListVDocs)))

-- | lookup a vdoc by its auid.
getVDoc :: AUID VDoc -> HandleResponse VDoc -> IO ()
getVDoc = request cfg (Proxy :: Proxy (RestAPIEndPointType ('EPVDoc 'EPGetVDoc)))

-- | create a new vdoc.
addVDoc :: ProtoVDoc -> HandleResponse VDoc -> IO ()
addVDoc = request cfg (Proxy :: Proxy (RestAPIEndPointType ('EPVDoc 'EPAddVDoc)))

changeTitleAndDesc :: AUID VDoc -> VDocTitleAndDesc -> HandleResponse VDocTitleAndDesc -> IO ()
changeTitleAndDesc = request cfg (Proxy :: Proxy (RestAPIEndPointType ('EPVDoc 'EPChangeTitleAndDesc)))

-- | change public flag of comment, note, etc..
changeNoteVisibility :: ChangeNoteVisibility -> HandleResponse () -> IO ()
changeNoteVisibility = request cfg (Proxy :: Proxy (RestAPIEndPointType ('EPVDoc 'EPChangeNoteVisibility)))

rmVDoc :: AUID VDoc -> HandleResponse VDoc -> IO ()
rmVDoc = request cfg (Proxy :: Proxy (RestAPIEndPointType ('EPVDoc 'EPRmVDoc)))

-- | get the document status at a given patch.  the resulting 'VDocVersion' includes mark tags of
-- all child patches.
getVersion :: PatchKey -> HandleResponse VDocVersion -> IO ()
getVersion = request cfg (Proxy :: Proxy (RestAPIEndPointType ('EPVDoc 'EPGetVersion)))

-- | create a new patch given a base patch and chunk range, new contents.
addPatch :: PatchKey -> PatchFromClient -> HandleResponse Patch -> IO ()
addPatch = request cfg (Proxy :: Proxy (RestAPIEndPointType ('EPVDoc 'EPAddPatch)))

rmPatch :: PatchKey -> HandleResponse Patch -> IO ()
rmPatch = request cfg (Proxy :: Proxy (RestAPIEndPointType ('EPVDoc 'EPRmPatch)))

-- | move _vdocHead to given patch.
mergePatch :: PatchKey -> HandleResponse Patch -> IO ()
mergePatch = request cfg (Proxy :: Proxy (RestAPIEndPointType ('EPVDoc 'EPMergePatch)))

-- | after having failed with 'mergePatch' above, you need to construct a conflict resolution object
-- and post it with this end-point.
rebasePatch :: AUID VDoc -> ConflictResolution -> HandleResponse VDoc -> IO ()
rebasePatch = request cfg (Proxy :: Proxy (RestAPIEndPointType ('EPVDoc 'EPRebasePatch)))

addComment :: PatchKey -> PatchCommentProto -> HandleResponse PatchComment -> IO ()
addComment = request cfg (Proxy :: Proxy (RestAPIEndPointType ('EPVDoc 'EPAddComment)))

rmComment :: PatchCommentKey -> HandleResponse PatchComment -> IO ()
rmComment = request cfg (Proxy :: Proxy (RestAPIEndPointType ('EPVDoc 'EPRmComment)))

addNote :: PatchKey -> PatchNoteProto -> HandleResponse PatchNote -> IO ()
addNote = request cfg (Proxy :: Proxy (RestAPIEndPointType ('EPVDoc 'EPAddNote)))

rmNote :: PatchNoteKey -> HandleResponse PatchNote -> IO ()
rmNote = request cfg (Proxy :: Proxy (RestAPIEndPointType ('EPVDoc 'EPRmNote)))

setVote :: PatchKey -> IdeaVoteValue -> HandleResponse PatchVote -> IO ()
setVote = request cfg (Proxy :: Proxy (RestAPIEndPointType ('EPVDoc 'EPSetVote)))

rmVote :: PatchVoteKey -> HandleResponse PatchVote -> IO ()
rmVote = request cfg (Proxy :: Proxy (RestAPIEndPointType ('EPVDoc 'EPRmVote)))
