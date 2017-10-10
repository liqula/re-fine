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
applyColorSchema _ (ButtonState Released NotRollOver) f = f BlackHack WhiteHack
applyColorSchema _ (ButtonState Released RollOver)    f = f BlackHack GreenHack
applyColorSchema _ (ButtonState Pressed  NotRollOver) f = f WhiteHack WhiteHack
applyColorSchema _ (ButtonState Pressed  RollOver)    f = f WhiteHack GreenHack
{-
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
-}

render :: ColorSchema -> ButtonState -> Icon -> ReactElementM h ()
render scm st ArrowDown = applyColorSchema scm st svgArrowDown
render scm st ArrowLeft = applyColorSchema scm st svgArrowLeft
render scm st ArrowRight = applyColorSchema scm st svgArrowRight
render scm st ArrowUp = applyColorSchema scm st svgArrowUp
render scm st Close = applyColorSchema scm st svgClose
render scm st Comment = applyColorSchema scm st svgComment
render scm st CommentNew = applyColorSchema scm st svgCommentNew
render scm st DiffCollapse = applyColorSchema scm st svgDiffCollapse
render scm st DiffDetails = applyColorSchema scm st svgDiffDetails
render scm st DiffExpand = applyColorSchema scm st svgDiffExpand
render scm st Discussion = applyColorSchema scm st (\c0 c1 -> svgDiscussion c0 c1 BlackHack)
render scm st DiscussionFlat = applyColorSchema scm st (\c0 _c1 -> svgDiscussionFlat c0)
render scm st DiscussionReply = applyColorSchema scm st (\c0 _c1 -> svgDiscussionReply c0)
render scm st DiscussionTree = applyColorSchema scm st (\c0 _c1 -> svgDiscussionTree c0)
render scm st DiscussionTreeChild = applyColorSchema scm st (\c0 _c1 -> svgDiscussionTreeChild c0)
render scm st Edit = applyColorSchema scm st (\c0 c1 -> svgEdit c0 c1 BlackHack)
render scm st EditKindGrammar = applyColorSchema scm st svgEditKindGrammar
render scm st EditKindMeaning = applyColorSchema scm st svgEditKindMeaning
render scm st EditKindPhrasing = applyColorSchema scm st svgEditKindPhrasing
render scm st EditNew = applyColorSchema scm st (\c0 _c1 -> svgEditNew c0 _c1 DiscussionDark)
render scm st EditReason = applyColorSchema scm st svgEditReason
render scm st EditToolbarBold = applyColorSchema scm st svgEditToolbarBold
render scm st EditToolbarBullets = applyColorSchema scm st svgEditToolbarBullets
render scm st EditToolbarH1 = applyColorSchema scm st svgEditToolbarH1
render scm st EditToolbarH2 = applyColorSchema scm st svgEditToolbarH2
render scm st EditToolbarH3 = applyColorSchema scm st svgEditToolbarH3
render scm st EditToolbarItalic = applyColorSchema scm st svgEditToolbarItalic
render scm st EditToolbarLink = applyColorSchema scm st svgEditToolbarLink
render scm st EditToolbarNumbers = applyColorSchema scm st svgEditToolbarNumbers
render scm st EditToolbarRedo = applyColorSchema scm st svgEditToolbarRedo
render scm st EditToolbarSave = applyColorSchema scm st svgEditToolbarSave
render scm st EditToolbarUndo = applyColorSchema scm st svgEditToolbarUndo
render scm st EditView = applyColorSchema scm st (\c0 _c1 -> svgEditView c0 _c1 DiscussionDark)
render scm st Faq = applyColorSchema scm st svgFaq
render scm st Filter = applyColorSchema scm st svgFilter
render scm st Group = applyColorSchema scm st  (\c0 c1 -> svgGroup c0 c1 BlackHack)
render scm st GroupNew = applyColorSchema scm st (\c0 _c1 -> svgGroupNew c0)
render scm st GroupUpdate = applyColorSchema scm st (\c0 _c1 -> svgGroupUpdate c0)
render scm st Help = applyColorSchema scm st svgHelp
render scm st Idea = applyColorSchema scm st svgIdea
render scm st IndexDesktop = applyColorSchema scm st (\c0 c1 -> svgIndexDesktop c0 c1 BlackHack)
render scm st IndexMobile = applyColorSchema scm st (\c0 _c1 -> svgIndexMobile c0)
render scm st Info = applyColorSchema scm st svgInfo
render scm st Lock = applyColorSchema scm st svgLock
render scm st Login = applyColorSchema scm st (\c0 c1 -> svgLogin c0 c1 BlackHack)
render scm st Note = applyColorSchema scm st svgNote
render scm st NotePrivate = applyColorSchema scm st (\c0 c1 -> svgNotePrivate c0 c1 DiscussionDark)
render scm st PhaseNext = applyColorSchema scm st svgPhaseNext
render scm st Play = applyColorSchema scm st svgPlay
render scm st Plus = applyColorSchema scm st svgPlus
render scm st Process = applyColorSchema scm st (\c0 c1 -> svgProcess c0 c1 BlackHack BlackHack)
render scm st ProcessNew = applyColorSchema scm st (\c0 _c1 -> svgProcessNew c0)
render scm st ProcessUpdate = applyColorSchema scm st (\c0 _c1 -> svgProcessUpdate c0)
render scm st Question = applyColorSchema scm st svgQuestion
render scm st Reader = applyColorSchema scm st svgReader
render scm st Reply = applyColorSchema scm st svgReply
render scm st Report = applyColorSchema scm st (\c0 _c1 -> svgReport c0)
render scm st Save = applyColorSchema scm st (\c0 _c1 -> svgSave c0)
render scm st Search = applyColorSchema scm st svgSearch
render scm st Share = applyColorSchema scm st (\c0 c1 -> svgShare c0 c1 BlackHack)
render scm st Sort = applyColorSchema scm st svgSort
render scm st User = applyColorSchema scm st svgUser
render scm st UserProfile = applyColorSchema scm st (\c0 c1 -> svgUserProfile c0 c1 BlackHack)
render scm st VoteNegative = applyColorSchema scm st svgVoteNegative
render scm st VoteNeutral = applyColorSchema scm st svgVoteNeutral
render scm st VotePositive = applyColorSchema scm st svgVotePositive
