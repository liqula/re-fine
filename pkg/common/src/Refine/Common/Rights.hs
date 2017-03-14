{-# LANGUAGE LambdaCase #-}
module Refine.Common.Rights where

import Refine.Common.Types.Access


-- What a user can do to a group
groupRights :: Role -> [Right]
groupRights = \case
  ReadOnly         -> [Read]
  Member           -> [Read]
  Moderator        -> [Read]
  LocalAdmin       -> [Read, Update]
  ProcessInitiator -> [Create, Read, Update, Delete]
  GroupInitiator   -> [Create, Read, Update, Delete]

-- What a user can do a document
documentRights :: Role -> [Right]
documentRights = \case
  ReadOnly         -> [Read]
  Member           -> [Create, Read, Update, Delete]
  Moderator        -> [Create, Read, Update, Delete]
  LocalAdmin       -> [Create, Read, Update, Delete]
  ProcessInitiator -> [Create, Read, Update, Delete]
  GroupInitiator   -> [Create, Read, Update, Delete]
