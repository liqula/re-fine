{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Contribution.Bubble
  ( bubble_
  , maybeStackProtoBubbles
  , stackProtoBubbles
  , stackComponents, StackOrNot(..)
  , constantBubbleHeight
  ) where
#include "import_frontend.hs"

import           Language.Css.Syntax
import           Web.HttpApiData (toUrlPiece)

import           Refine.Common.Types
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Icon
import qualified Refine.Frontend.Icon.Svg as Svg
import           Refine.Frontend.Screen.Calculations
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store()
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Types
import           Refine.Frontend.Util


mkClickHandler :: HasCallStack => [ContributionAction] -> Event -> MouseEvent -> (ViewEventHandler, [EventModification])
mkClickHandler actions _ _ = simpleHandler . mconcat $ dispatch . ContributionAction <$> actions

bubbleStackStyles :: HasCallStack => [Decl]
bubbleStackStyles = [decl "border" (Ident "3px dotted black")]

bubble :: HasCallStack => ReactElementM 'EventHandlerCode () -> View '[BubbleProps]
bubble children = mkView "Bubble" $ \props -> do
  let bubbleKind = case props ^. bubblePropsContributionIds of
          NoStack (ContribIDDiscussion True  _, _) -> Left "o-snippet--note"
          NoStack (ContribIDDiscussion False _, _) -> Left "o-snippet--discussion"
          NoStack (ContribIDEdit _, _)             -> Left "o-snippet--edit"
          Stack _                                  -> Right bubbleStackStyles

      clickActions = case props ^. bubblePropsContributionIds of
          NoStack (cid, _)
            -> [ ShowContributionDialog cid
               , HighlightMarkAndBubble []    -- question: should this be done by the store update function of ShowContributionDialog?
               ]
          Stack (cid :| cids)
            -> [SetBubbleFilter . Just . Set.fromList . fmap fst $ cid : cids, SetBubblePositioning BubblePositioningEvenlySpaced]

      bubbleClasses :: [JSString]
      bubbleClasses
          = "o-snippet"  -- RENAME: snippet => bubble
          : either (:[]) (const []) bubbleKind
         <> ["o-snippet--hover" | props ^. bubblePropsHighlight]

      bubbleStyles :: [Decl]
      bubbleStyles
          = either (const []) id bubbleKind
         <> verticalPosition (props ^. bubblePropsVerticalOffset) (props ^. bubblePropsScreenState)

      -- it is not ok to have more than one "style" or "className" attribute in this list.
      attrs =
       [ "style" @@= bubbleStyles
       , "className" $= toClasses bubbleClasses

       , onClick      $ mkClickHandler clickActions
       , onMouseEnter $ mkClickHandler [HighlightMarkAndBubble . map (uncurry MarkContribution) . stackToList $ props ^. bubblePropsContributionIds]
       , onMouseLeave $ mkClickHandler [HighlightMarkAndBubble []]
       ]

  div_ attrs $ do
    div_ ["className" $= cs ("o-snippet__icon-bg o-snippet__icon-bg--" <> show (props ^. bubblePropsIconSide))] $ do  -- RENAME: snippet => bubble
      let img = case props ^. bubblePropsContributionIds of
            NoStack (ContribIDDiscussion _ _, _) -> Svg.Discussion
            NoStack (ContribIDEdit _, _)         -> Svg.Edit
            Stack _                              -> Svg.Edit  -- TODO: new svg icon 'Stack'.
      ibutton_ $ emptyIbuttonProps (ButtonImageIcon img ColorSchemaDark) ([] :: [GlobalAction]) & ibSize .~ Medium
    div_ ["className" $= "o-snippet__content"]  -- RENAME: snippet => bubble
      children

bubble_ :: HasCallStack => BubbleProps -> ReactElementM 'EventHandlerCode () -> ReactElementM 'EventHandlerCode ()
bubble_ props children = view_ (bubble children) (bubbleKey props) props
  -- (there is React.Flux.Internal.childrenPassedToView, but doing it by hand is easier to understand.)

bubbleKey :: HasCallStack => BubbleProps -> JSString
bubbleKey props = "bubble_" <> props ^. bubblePropsContributionIds . to (cs . toUrlPiece . uncurry MarkContribution . stackToHead)

verticalPosition :: HasCallStack => Maybe OffsetFromDocumentTop -> ScreenState -> [Decl]
verticalPosition Nothing       _  = [decl "marginTop" (Px 20), decl "position" (Ident "relative")]
verticalPosition (Just offset) st = [decl "top" (Px $ offsetIntoText offset st), decl "position" (Ident "absolute")]


-- * stacking

-- | FUTUREWORK: it would be nice to get around this, but as long as it's true, it makes things a
-- lot easier...
constantBubbleHeight :: HasCallStack => OffsetFromDocumentTop
constantBubbleHeight = 81

maybeStackProtoBubbles :: HasCallStack => BubblePositioning -> [ProtoBubble] -> [StackOrNot ProtoBubble]
maybeStackProtoBubbles BubblePositioningAbsolute     = stackProtoBubbles
maybeStackProtoBubbles BubblePositioningEvenlySpaced = fmap NoStack

stackProtoBubbles :: HasCallStack => [ProtoBubble] -> [StackOrNot ProtoBubble]
stackProtoBubbles = stackComponents getTop getHeight
  where
    getTop    = view (protoBubbleVerticalSpanBounds . verticalSpanBoundsTop . unOffsetFromDocumentTop)
    getHeight = const (constantBubbleHeight ^. unOffsetFromDocumentTop)
        -- (we could use 'verticalSpanBoundsBottom' here, but that's awkward and yields the same result.)

-- | given a list of abstract components together with their absolute position and height, group all
-- overlapping components into stacks, and leave all others single.
stackComponents :: HasCallStack => forall pos height comp. (pos ~ Int, height ~ Int)
                => (comp -> pos) -> (comp -> height)
                -> [comp] -> [StackOrNot comp]
stackComponents getPos getHeight comps = assert (all (\c -> getPos c >= 0 && getHeight c >= 1) comps)
                                       . go 0 [] . sortBy (compare `on` getPos)
                                       $ comps
  where
    go _ [] []
      = []

    -- start a new pile if the old one is empty.
    go usedHeight [] (x : xs)
      = go (updateUsedHeight usedHeight x) [x] xs

    -- if the next one *does* overlap with the (non-empty) pile, add it.
    go usedHeight overlapping@(_:_) (x : xs) | getPos x <= usedHeight
      = go (updateUsedHeight usedHeight x) (x : overlapping) xs

    -- if the next one *does not* overlap with the (non-empty) pile, emit a new 'StackOrNot'
    go usedHeight (overlapping : overlappings) xs
      = save (overlapping :| overlappings) : go usedHeight [] xs

    updateUsedHeight old x = max old (getPos x + getHeight x)

    save cps@(_ :| (_:_)) = Stack (NEL.reverse cps)
    save (cp :| _)        = NoStack cp
