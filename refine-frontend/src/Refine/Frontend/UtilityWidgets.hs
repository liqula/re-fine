{-# LANGUAGE OverloadedStrings #-}

module Refine.Frontend.UtilityWidgets where

import           Data.Char (toLower)
import           Data.Maybe (fromJust, isNothing)
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           GHCJS.Types (JSString)
import           React.Flux

import           Refine.Frontend.Style

icon :: ReactView JSString
icon = defineView "Icon" $ \iconClass ->
  div_ ["className" $= iconClass] $ do
      span_ ["className" $= "path1"] ""
      span_ ["className" $= "path2"] ""
      span_ ["className" $= "path3"] ""
      span_ ["className" $= "path4"] ""
      span_ ["className" $= "path5"] ""
      span_ ["className" $= "path6"] ""
      span_ ["className" $= "path7"] ""
      span_ ["className" $= "path8"] ""

data IconSize = S
    | M
    | L
    | XL
    | XXL
    deriving Show

icon_ :: JSString -> ReactElementM eventHandler ()
icon_ iconClass = view icon iconClass mempty

data IconButtonWithAlignmentProps = IconButtonWithAlignmentProps
    { _iconButtonProps :: IconButtonProps
    , _rightAligned :: Bool
    , _position :: Maybe Int
    }

iconButtonWithAlignment :: ReactView IconButtonWithAlignmentProps
iconButtonWithAlignment = defineStatefulView "IconButtonWithAlignment" False $ \mouseIsOver (IconButtonWithAlignmentProps (IconButtonProps blockName elementName contentType iconHighlight (iconName, iconType) label iconsize) rightAligned position) ->
    button_ (["data-content-type" $= contentType
           , "className" $= fromString (concat [blockName, "__button ",
                                        blockName, if elementName == "" then "" else "__", elementName,
                                        alignmentClass blockName rightAligned])
           , onMouseEnter $ \_ _ _ -> ([], Just True)
           , onMouseLeave $ \_ _ _ -> ([], Just False)
           ] <> if isNothing position then [] else ["style" @= [Style "top" (fromJust position)]]) $ do
        div_ ["className" $= fromString (blockName <> "__icon")] $ do
            icon_ $ fromString ((if iconHighlight then "o-icon-highlight " else "") <>
                    iconName <> "_" <> (if mouseIsOver && iconHighlight then "RO" else iconType) <>
                    " " <> "iconsize-" <> map toLower (show iconsize))
        span_ ["className" $= fromString (blockName <> "__button-label")] $ elemJSString label
    where alignmentClass blockName' rightAligned' = if rightAligned' then " " <> blockName' <> "--align-right" else ""

iconButtonWithAlignment_ :: IconButtonWithAlignmentProps -> ReactElementM eventHandler ()
iconButtonWithAlignment_ props = view iconButtonWithAlignment props mempty

data IconButtonProps = IconButtonProps
    { _blockName :: String
    , _elementName :: String
    , _contentType :: JSString
    , _iconHighlight :: Bool
    , _iconDesc :: (String, String)
    , _label :: JSString
    , _size :: IconSize
    }

iconButton :: ReactView IconButtonProps
iconButton = defineView "IconButton" $ \props ->
    iconButtonWithAlignment_ $ IconButtonWithAlignmentProps props rightAligned Nothing
    where rightAligned = False -- no right alignment in the standard case

iconButton_ :: IconButtonProps -> ReactElementM eventHandler ()
iconButton_ props = view iconButton props mempty

positionedIconButton :: ReactView (IconButtonProps, Int)
positionedIconButton = defineView "IconButton" $ \(props, position) ->
    iconButtonWithAlignment_ $ IconButtonWithAlignmentProps props rightAligned (Just position)
    where rightAligned = False -- no right alignment in the standard case

positionedIconButton_ :: IconButtonProps -> Int -> ReactElementM eventHandler ()
positionedIconButton_ props position = view positionedIconButton (props, position) mempty
