{-# LANGUAGE CPP #-}
#include "language_frontend.hs"
module Refine.Frontend.Icon.Svg
  ( ColorSchema(..)
  , ButtonState(..)
  , ButtonRollOver(..)
  , Icon(..)
  , render
  ) where
#include "import_frontend.hs"

import Refine.Common.Color
import Refine.Frontend.Icon.Svg.Internal


data ColorSchema
  = ColorSchemaBright
  | ColorSchemaDark
  | ColorSchemaNote
  | ColorSchemaDiscussion
  | ColorSchemaEdit
  deriving (Eq, Show, Generic, Bounded, Enum)

-- | (See {-TODO-} for disabled/grayed out.)
data ButtonState = Released | Pressed
  deriving (Eq, Show, Generic, Bounded, Enum)

data ButtonRollOver = RollOver | NotRollOver
  deriving (Eq, Show, Generic, Bounded, Enum)


applyColorSchema :: ColorSchema -> ButtonState -> ButtonRollOver -> (Color -> Color -> a) -> a
applyColorSchema ColorSchemaBright     Released NotRollOver f = f (error "applyColorSchema") (error "applyColorSchema")
applyColorSchema ColorSchemaBright     Released RollOver    f = f (error "applyColorSchema") (error "applyColorSchema")
applyColorSchema ColorSchemaBright     Pressed  NotRollOver f = f (error "applyColorSchema") (error "applyColorSchema")
applyColorSchema ColorSchemaBright     Pressed  RollOver    f = f (error "applyColorSchema") (error "applyColorSchema")
applyColorSchema ColorSchemaDark       Released NotRollOver f = f (error "applyColorSchema") (error "applyColorSchema")
applyColorSchema ColorSchemaDark       Released RollOver    f = f (error "applyColorSchema") (error "applyColorSchema")
applyColorSchema ColorSchemaDark       Pressed  NotRollOver f = f (error "applyColorSchema") (error "applyColorSchema")
applyColorSchema ColorSchemaDark       Pressed  RollOver    f = f (error "applyColorSchema") (error "applyColorSchema")
applyColorSchema ColorSchemaNote       Released NotRollOver f = f (error "applyColorSchema") (error "applyColorSchema")
applyColorSchema ColorSchemaNote       Released RollOver    f = f (error "applyColorSchema") (error "applyColorSchema")
applyColorSchema ColorSchemaNote       Pressed  NotRollOver f = f (error "applyColorSchema") (error "applyColorSchema")
applyColorSchema ColorSchemaNote       Pressed  RollOver    f = f (error "applyColorSchema") (error "applyColorSchema")
applyColorSchema ColorSchemaDiscussion Released NotRollOver f = f (error "applyColorSchema") (error "applyColorSchema")
applyColorSchema ColorSchemaDiscussion Released RollOver    f = f (error "applyColorSchema") (error "applyColorSchema")
applyColorSchema ColorSchemaDiscussion Pressed  NotRollOver f = f (error "applyColorSchema") (error "applyColorSchema")
applyColorSchema ColorSchemaDiscussion Pressed  RollOver    f = f (error "applyColorSchema") (error "applyColorSchema")
applyColorSchema ColorSchemaEdit       Released NotRollOver f = f (error "applyColorSchema") (error "applyColorSchema")
applyColorSchema ColorSchemaEdit       Released RollOver    f = f (error "applyColorSchema") (error "applyColorSchema")
applyColorSchema ColorSchemaEdit       Pressed  NotRollOver f = f (error "applyColorSchema") (error "applyColorSchema")
applyColorSchema ColorSchemaEdit       Pressed  RollOver    f = f (error "applyColorSchema") (error "applyColorSchema")


render :: ColorSchema -> ButtonState -> ButtonRollOver -> Icon -> ReactElementM h ()
render scm st ro ArrowDown = applyColorSchema scm st ro (\c0 _c1 -> svgArrowDown c0)
render scm st ro ArrowLeft = applyColorSchema scm st ro (\c0 _c1 -> svgArrowLeft c0)
render scm st ro ArrowRight = applyColorSchema scm st ro (\c0 _c1 -> svgArrowRight c0)
render scm st ro ArrowUp = applyColorSchema scm st ro (\c0 _c1 -> svgArrowUp c0)
render scm st ro CatArt = applyColorSchema scm st ro (\c0 _c1 -> svgCatArt c0)
render scm st ro CatBusiness = applyColorSchema scm st ro (\c0 _c1 -> svgCatBusiness c0)
render scm st ro CatFitness = applyColorSchema scm st ro (\c0 _c1 -> svgCatFitness c0)
render scm st ro CatFlowers = applyColorSchema scm st ro (\c0 _c1 -> svgCatFlowers c0)
render scm st ro CatParty = applyColorSchema scm st ro (\c0 _c1 -> svgCatParty c0)
render scm st ro CatPhoto = applyColorSchema scm st ro (\c0 _c1 -> svgCatPhoto c0)
render scm st ro CatRollerdisco = applyColorSchema scm st ro (\c0 _c1 -> svgCatRollerdisco c0)
render scm st ro CatRomantic = applyColorSchema scm st ro (\c0 _c1 -> svgCatRomantic c0)
render scm st ro CatSailing = applyColorSchema scm st ro (\c0 _c1 -> svgCatSailing c0)
render scm st ro CatWork = applyColorSchema scm st ro (\c0 _c1 -> svgCatWork c0)
render scm st ro Close = applyColorSchema scm st ro (\c0 _c1 -> svgClose c0)
render scm st ro Comment = applyColorSchema scm st ro (\c0 _c1 -> svgComment c0)
render scm st ro CommentNew = applyColorSchema scm st ro (\c0 c1 -> svgCommentNew c0 c1)
render scm st ro DiffCollapse = applyColorSchema scm st ro (\c0 _c1 -> svgDiffCollapse c0)
render scm st ro DiffDetails = applyColorSchema scm st ro (\c0 _c1 -> svgDiffDetails c0)
render scm st ro DiffExpand = applyColorSchema scm st ro (\c0 _c1 -> svgDiffExpand c0)
render scm st ro Discussion = applyColorSchema scm st ro (\c0 c1 -> svgDiscussion c0 c1)
render scm st ro DiscussionFlat = applyColorSchema scm st ro (\c0 _c1 -> svgDiscussionFlat c0)
render scm st ro DiscussionTree = applyColorSchema scm st ro (\c0 _c1 -> svgDiscussionTree c0)
render scm st ro Edit = applyColorSchema scm st ro (\c0 c1 -> svgEdit c0 c1)
render scm st ro EditKindGrammar = applyColorSchema scm st ro (\c0 _c1 -> svgEditKindGrammar c0)
render scm st ro EditKindMeaning = applyColorSchema scm st ro (\c0 _c1 -> svgEditKindMeaning c0)
render scm st ro EditKindPhrasing = applyColorSchema scm st ro (\c0 _c1 -> svgEditKindPhrasing c0)
render scm st ro EditNew = applyColorSchema scm st ro (\c0 _c1 -> svgEditNew c0 _c1 DiscussionDark)
render scm st ro EditReason = applyColorSchema scm st ro (\c0 _c1 -> svgEditReason c0)
render scm st ro EditToolbarBold = applyColorSchema scm st ro (\c0 _c1 -> svgEditToolbarBold c0)
render scm st ro EditToolbarBullets = applyColorSchema scm st ro (\c0 _c1 -> svgEditToolbarBullets c0)
render scm st ro EditToolbarH1 = applyColorSchema scm st ro (\c0 _c1 -> svgEditToolbarH1 c0)
render scm st ro EditToolbarH2 = applyColorSchema scm st ro (\c0 _c1 -> svgEditToolbarH2 c0)
render scm st ro EditToolbarH3 = applyColorSchema scm st ro (\c0 _c1 -> svgEditToolbarH3 c0)
render scm st ro EditToolbarItalic = applyColorSchema scm st ro (\c0 _c1 -> svgEditToolbarItalic c0)
render scm st ro EditToolbarLink = applyColorSchema scm st ro (\c0 _c1 -> svgEditToolbarLink c0)
render scm st ro EditToolbarNumbers = applyColorSchema scm st ro (\c0 _c1 -> svgEditToolbarNumbers c0)
render scm st ro EditToolbarRedo = applyColorSchema scm st ro (\c0 _c1 -> svgEditToolbarRedo c0)
render scm st ro EditToolbarSave = applyColorSchema scm st ro (\c0 _c1 -> svgEditToolbarSave c0)
render scm st ro EditToolbarUndo = applyColorSchema scm st ro (\c0 _c1 -> svgEditToolbarUndo c0)
render scm st ro EditView = applyColorSchema scm st ro (\c0 _c1 -> svgEditView c0 _c1 DiscussionDark)
render scm st ro Faq = applyColorSchema scm st ro (\c0 _c1 -> svgFaq c0)
render scm st ro Filter = applyColorSchema scm st ro (\c0 _c1 -> svgFilter c0)
render scm st ro Group = applyColorSchema scm st ro (\c0 _c1 -> svgGroup c0)
render scm st ro GroupNew = applyColorSchema scm st ro (\c0 _c1 -> svgGroupNew c0)
render scm st ro GroupUpdate = applyColorSchema scm st ro (\c0 _c1 -> svgGroupUpdate c0)
render scm st ro Help = applyColorSchema scm st ro (\c0 _c1 -> svgHelp c0)
render scm st ro Idea = applyColorSchema scm st ro (\c0 _c1 -> svgIdea c0)
render scm st ro IndexDesktop = applyColorSchema scm st ro (\c0 _c1 -> svgIndexDesktop c0)
render scm st ro IndexMobile = applyColorSchema scm st ro (\c0 _c1 -> svgIndexMobile c0)
render scm st ro Info = applyColorSchema scm st ro (\c0 c1 -> svgInfo c0 c1)
render scm st ro Lock = applyColorSchema scm st ro (\c0 c1 -> svgLock c0 c1)
render scm st ro Login = applyColorSchema scm st ro (\c0 c1 -> svgLogin c0 c1)
render scm st ro Note = applyColorSchema scm st ro (\c0 _c1 -> svgNote c0)
render scm st ro NotePrivate = applyColorSchema scm st ro (\c0 c1 -> svgNotePrivate c0 c1 DiscussionDark)
render scm st ro PhaseNext = applyColorSchema scm st ro (\c0 _c1 -> svgPhaseNext c0)
render scm st ro Play = applyColorSchema scm st ro (\c0 _c1 -> svgPlay c0)
render scm st ro Plus = applyColorSchema scm st ro (\c0 _c1 -> svgPlus c0)
render scm st ro Process = applyColorSchema scm st ro (\c0 _c1 -> svgProcess c0)
render scm st ro ProcessNew = applyColorSchema scm st ro (\c0 _c1 -> svgProcessNew c0)
render scm st ro ProcessUpdate = applyColorSchema scm st ro (\c0 _c1 -> svgProcessUpdate c0)
render scm st ro Question = applyColorSchema scm st ro (\c0 _c1 -> svgQuestion c0)
render scm st ro Reader = applyColorSchema scm st ro (\c0 _c1 -> svgReader c0)
render scm st ro Reply = applyColorSchema scm st ro (\c0 _c1 -> svgReply c0)
render scm st ro Save = applyColorSchema scm st ro (\c0 _c1 -> svgSave c0)
render scm st ro Search = applyColorSchema scm st ro (\c0 _c1 -> svgSearch c0)
render scm st ro Share = applyColorSchema scm st ro (\c0 c1 -> svgShare c0 c1)
render scm st ro Sort = applyColorSchema scm st ro (\c0 _c1 -> svgSort c0)
render scm st ro User = applyColorSchema scm st ro (\c0 _c1 -> svgUser c0)
render scm st ro UserProfile = applyColorSchema scm st ro (\c0 _c1 -> svgUserProfile c0)
render scm st ro VoteNegative = applyColorSchema scm st ro (\c0 c1 -> svgVoteNegative c0 c1)
render scm st ro VoteNeutral = applyColorSchema scm st ro (\c0 c1 -> svgVoteNeutral c0 c1)
render scm st ro VotePositive = applyColorSchema scm st ro (\c0 c1 -> svgVotePositive c0 c1)
