{-# LANGUAGE CPP #-}
#include "language_frontend.hs"
module Refine.Frontend.Icon.Svg
  ( ColorSchema(..)
  , ButtonState(..), buttonPressed, buttonRollOver
  , ButtonPressed(..)
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

-- | (Related: '_ibEnabled'.)
data ButtonState = ButtonState
  { _buttonPressed  :: ButtonPressed
  , _buttonRollOver :: ButtonRollOver
  }
  deriving (Eq, Show, Generic)

data ButtonPressed = Released | Pressed
  deriving (Eq, Show, Generic, Bounded, Enum)

data ButtonRollOver = NotRollOver | RollOver
  deriving (Eq, Show, Generic, Bounded, Enum)

deriveClasses [([''ButtonState, ''ButtonPressed, ''ButtonRollOver], allClass)]

instance Default ButtonState where
  def = ButtonState Released NotRollOver


applyColorSchema :: ColorSchema -> ButtonState -> (Color -> Color -> a) -> a
applyColorSchema ColorSchemaBright     (ButtonState Released NotRollOver) f = f BlueDark BlueLight
applyColorSchema ColorSchemaBright     (ButtonState Released RollOver)    f = f BlueLight BlueDark
applyColorSchema ColorSchemaBright     (ButtonState Pressed  NotRollOver) f = f BlueLight BlueDark
applyColorSchema ColorSchemaBright     (ButtonState Pressed  RollOver)    f = f BlueDark BlueLight
applyColorSchema ColorSchemaDark       (ButtonState Released NotRollOver) f = f BlueLight BlueDark
applyColorSchema ColorSchemaDark       (ButtonState Released RollOver)    f = f BlueDark BlueLight
applyColorSchema ColorSchemaDark       (ButtonState Pressed  NotRollOver) f = f BlueDark BlueLight
applyColorSchema ColorSchemaDark       (ButtonState Pressed  RollOver)    f = f BlueLight BlueDark
applyColorSchema ColorSchemaNote       (ButtonState Released NotRollOver) f = f NoteDark BlueLight
applyColorSchema ColorSchemaNote       (ButtonState Released RollOver)    f = f BlueLight NoteDark
applyColorSchema ColorSchemaNote       (ButtonState Pressed  NotRollOver) f = f BlueLight NoteDark
applyColorSchema ColorSchemaNote       (ButtonState Pressed  RollOver)    f = f NoteDark BlueLight
applyColorSchema ColorSchemaDiscussion (ButtonState Released NotRollOver) f = f DiscussionDark BlueLight
applyColorSchema ColorSchemaDiscussion (ButtonState Released RollOver)    f = f BlueLight DiscussionDark
applyColorSchema ColorSchemaDiscussion (ButtonState Pressed  NotRollOver) f = f BlueLight DiscussionDark
applyColorSchema ColorSchemaDiscussion (ButtonState Pressed  RollOver)    f = f DiscussionDark BlueLight
applyColorSchema ColorSchemaEdit       (ButtonState Released NotRollOver) f = f EditDark BlueLight
applyColorSchema ColorSchemaEdit       (ButtonState Released RollOver)    f = f BlueLight EditDark
applyColorSchema ColorSchemaEdit       (ButtonState Pressed  NotRollOver) f = f BlueLight EditDark
applyColorSchema ColorSchemaEdit       (ButtonState Pressed  RollOver)    f = f EditDark BlueLight


render :: ColorSchema -> ButtonState -> Icon -> ReactElementM h ()
render scm st ArrowDown = applyColorSchema scm st (\c0 _c1 -> svgArrowDown c0)
render scm st ArrowLeft = applyColorSchema scm st (\c0 _c1 -> svgArrowLeft c0)
render scm st ArrowRight = applyColorSchema scm st (\c0 _c1 -> svgArrowRight c0)
render scm st ArrowUp = applyColorSchema scm st (\c0 _c1 -> svgArrowUp c0)
render scm st CatArt = applyColorSchema scm st (\c0 _c1 -> svgCatArt c0)
render scm st CatBusiness = applyColorSchema scm st (\c0 _c1 -> svgCatBusiness c0)
render scm st CatFitness = applyColorSchema scm st (\c0 _c1 -> svgCatFitness c0)
render scm st CatFlowers = applyColorSchema scm st (\c0 _c1 -> svgCatFlowers c0)
render scm st CatParty = applyColorSchema scm st (\c0 _c1 -> svgCatParty c0)
render scm st CatPhoto = applyColorSchema scm st (\c0 _c1 -> svgCatPhoto c0)
render scm st CatRollerdisco = applyColorSchema scm st (\c0 _c1 -> svgCatRollerdisco c0)
render scm st CatRomantic = applyColorSchema scm st (\c0 _c1 -> svgCatRomantic c0)
render scm st CatSailing = applyColorSchema scm st (\c0 _c1 -> svgCatSailing c0)
render scm st CatWork = applyColorSchema scm st (\c0 _c1 -> svgCatWork c0)
render scm st Close = applyColorSchema scm st (\c0 _c1 -> svgClose c0)
render scm st Comment = applyColorSchema scm st (\c0 _c1 -> svgComment c0)
render scm st CommentNew = applyColorSchema scm st (\c0 c1 -> svgCommentNew c0 c1)
render scm st DiffCollapse = applyColorSchema scm st (\c0 _c1 -> svgDiffCollapse c0)
render scm st DiffDetails = applyColorSchema scm st (\c0 _c1 -> svgDiffDetails c0)
render scm st DiffExpand = applyColorSchema scm st (\c0 _c1 -> svgDiffExpand c0)
render scm st Discussion = applyColorSchema scm st (\c0 c1 -> svgDiscussion c0 c1)
render scm st DiscussionFlat = applyColorSchema scm st (\c0 _c1 -> svgDiscussionFlat c0)
render scm st DiscussionTree = applyColorSchema scm st (\c0 _c1 -> svgDiscussionTree c0)
render scm st Edit = applyColorSchema scm st (\c0 c1 -> svgEdit c0 c1)
render scm st EditKindGrammar = applyColorSchema scm st (\c0 _c1 -> svgEditKindGrammar c0)
render scm st EditKindMeaning = applyColorSchema scm st (\c0 _c1 -> svgEditKindMeaning c0)
render scm st EditKindPhrasing = applyColorSchema scm st (\c0 _c1 -> svgEditKindPhrasing c0)
render scm st EditNew = applyColorSchema scm st (\c0 _c1 -> svgEditNew c0 _c1 DiscussionDark)
render scm st EditReason = applyColorSchema scm st (\c0 _c1 -> svgEditReason c0)
render scm st EditToolbarBold = applyColorSchema scm st (\c0 _c1 -> svgEditToolbarBold c0)
render scm st EditToolbarBullets = applyColorSchema scm st (\c0 _c1 -> svgEditToolbarBullets c0)
render scm st EditToolbarH1 = applyColorSchema scm st (\c0 _c1 -> svgEditToolbarH1 c0)
render scm st EditToolbarH2 = applyColorSchema scm st (\c0 _c1 -> svgEditToolbarH2 c0)
render scm st EditToolbarH3 = applyColorSchema scm st (\c0 _c1 -> svgEditToolbarH3 c0)
render scm st EditToolbarItalic = applyColorSchema scm st (\c0 _c1 -> svgEditToolbarItalic c0)
render scm st EditToolbarLink = applyColorSchema scm st (\c0 _c1 -> svgEditToolbarLink c0)
render scm st EditToolbarNumbers = applyColorSchema scm st (\c0 _c1 -> svgEditToolbarNumbers c0)
render scm st EditToolbarRedo = applyColorSchema scm st (\c0 _c1 -> svgEditToolbarRedo c0)
render scm st EditToolbarSave = applyColorSchema scm st (\c0 _c1 -> svgEditToolbarSave c0)
render scm st EditToolbarUndo = applyColorSchema scm st (\c0 _c1 -> svgEditToolbarUndo c0)
render scm st EditView = applyColorSchema scm st (\c0 _c1 -> svgEditView c0 _c1 DiscussionDark)
render scm st Faq = applyColorSchema scm st (\c0 _c1 -> svgFaq c0)
render scm st Filter = applyColorSchema scm st (\c0 _c1 -> svgFilter c0)
render scm st Group = applyColorSchema scm st (\c0 _c1 -> svgGroup c0)
render scm st GroupNew = applyColorSchema scm st (\c0 _c1 -> svgGroupNew c0)
render scm st GroupUpdate = applyColorSchema scm st (\c0 _c1 -> svgGroupUpdate c0)
render scm st Help = applyColorSchema scm st (\c0 _c1 -> svgHelp c0)
render scm st Idea = applyColorSchema scm st (\c0 _c1 -> svgIdea c0)
render scm st IndexDesktop = applyColorSchema scm st (\c0 _c1 -> svgIndexDesktop c0)
render scm st IndexMobile = applyColorSchema scm st (\c0 _c1 -> svgIndexMobile c0)
render scm st Info = applyColorSchema scm st (\c0 c1 -> svgInfo c0 c1)
render scm st Lock = applyColorSchema scm st (\c0 c1 -> svgLock c0 c1)
render scm st Login = applyColorSchema scm st (\c0 c1 -> svgLogin c0 c1)
render scm st Note = applyColorSchema scm st (\c0 _c1 -> svgNote c0)
render scm st NotePrivate = applyColorSchema scm st (\c0 c1 -> svgNotePrivate c0 c1 DiscussionDark)
render scm st PhaseNext = applyColorSchema scm st (\c0 _c1 -> svgPhaseNext c0)
render scm st Play = applyColorSchema scm st (\c0 _c1 -> svgPlay c0)
render scm st Plus = applyColorSchema scm st (\c0 _c1 -> svgPlus c0)
render scm st Process = applyColorSchema scm st (\c0 _c1 -> svgProcess c0)
render scm st ProcessNew = applyColorSchema scm st (\c0 _c1 -> svgProcessNew c0)
render scm st ProcessUpdate = applyColorSchema scm st (\c0 _c1 -> svgProcessUpdate c0)
render scm st Question = applyColorSchema scm st (\c0 _c1 -> svgQuestion c0)
render scm st Reader = applyColorSchema scm st (\c0 _c1 -> svgReader c0)
render scm st Reply = applyColorSchema scm st (\c0 _c1 -> svgReply c0)
render scm st Save = applyColorSchema scm st (\c0 _c1 -> svgSave c0)
render scm st Search = applyColorSchema scm st (\c0 _c1 -> svgSearch c0)
render scm st Share = applyColorSchema scm st (\c0 c1 -> svgShare c0 c1)
render scm st Sort = applyColorSchema scm st (\c0 _c1 -> svgSort c0)
render scm st User = applyColorSchema scm st (\c0 _c1 -> svgUser c0)
render scm st UserProfile = applyColorSchema scm st (\c0 _c1 -> svgUserProfile c0)
render scm st VoteNegative = applyColorSchema scm st (\c0 c1 -> svgVoteNegative c0 c1)
render scm st VoteNeutral = applyColorSchema scm st (\c0 c1 -> svgVoteNeutral c0 c1)
render scm st VotePositive = applyColorSchema scm st (\c0 c1 -> svgVotePositive c0 c1)
