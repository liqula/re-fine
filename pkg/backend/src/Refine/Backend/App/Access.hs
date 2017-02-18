module Refine.Backend.App.Access where

import Refine.Backend.App.Core
import Refine.Backend.Database.Core (DB)
import Refine.Backend.Database.Class
import Refine.Common.Types


changeAccess :: ChangeAccess -> App DB ()
changeAccess (ChangeAccess ad a u) = case ad of
  AccessibleNote       nid -> changeNoteAccess nid a u
  AccessibleDiscussion did -> changeDiscussionAccess did a u
  AccessibleQuestion   qid -> changeQuestionAccess qid a u

changeNoteAccess :: ID Note -> Access -> ID User -> App DB ()
changeNoteAccess nid a uid = do
  appLog $ unwords [show a, " access on note ", show nid, " for ", show uid]
  db $ case a of
    Grant  -> addNoteUserAccess nid uid
    Revoke -> removeNoteUserAccess nid uid

changeDiscussionAccess :: ID Discussion -> Access -> ID User -> App DB ()
changeDiscussionAccess did a uid = do
  appLog $ unwords [show a, " access on discussion ", show did, " for ", show uid]
  db $ case a of
    Grant  -> addDiscussionUserAccess did uid
    Revoke -> removeDiscussionUserAccess did uid

changeQuestionAccess :: ID Question -> Access -> ID User -> App DB ()
changeQuestionAccess qid a uid = do
  appLog $ unwords [show a, " access on question ", show qid, " for ", show uid]
  db $ case a of
    Grant  -> addQuestionUserAccess qid uid
    Revoke -> removeQuestionUserAccess qid uid
