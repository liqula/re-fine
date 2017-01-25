{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Refine.Frontend.UtilityWidgets where

import           Control.Lens (makeLenses, (^.), _1, _2)
import           Data.Char (toLower)
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           GHCJS.Types (JSString)
import           React.Flux

import           Refine.Frontend.Style


data IconSize
    = S
    | M
    | L
    | XL
    | XXL
    deriving Show

data IconProps = IconProps
  { _blockName     :: String
  , _iconHighlight :: Bool
  , _iconDesc      :: (String, String)
  , _size          :: IconSize

  }

makeLenses ''IconProps

data IconButtonProps = IconButtonProps
  { _iconProps     :: IconProps
  , _elementName   :: String
  , _moduleName    :: String
  , _contentType   :: JSString
  , _label         :: JSString
  , _clickHandler    :: ClickHandler
  }

type ClickHandler = Event -> MouseEvent -> [SomeStoreAction]

makeLenses ''IconButtonProps

data IconButtonWithAlignmentProps = IconButtonWithAlignmentProps
    { _iconButtonProps :: IconButtonProps
    , _rightAligned    :: Bool
    , _position        :: Maybe Int
    }

makeLenses ''IconButtonWithAlignmentProps


iconCore :: ReactView JSString
iconCore = defineView "IconCore" $ \iconClass ->
  div_ ["className" $= iconClass] $ do
    span_ ["className" $= "path1"] ""
    span_ ["className" $= "path2"] ""
    span_ ["className" $= "path3"] ""
    span_ ["className" $= "path4"] ""
    span_ ["className" $= "path5"] ""
    span_ ["className" $= "path6"] ""
    span_ ["className" $= "path7"] ""
    span_ ["className" $= "path8"] ""

iconCore_ :: JSString -> ReactElementM eventHandler ()
iconCore_ iconClass = view iconCore iconClass mempty


icon :: ReactView IconProps
icon = defineStatefulView "Icon" False $ \mouseIsOver props -> do
  -- TODO unify the naming schemas of the classes of the different icons!
  let -- TODO: these could do with better names
    a = if props ^. iconHighlight then "o-icon-highlight " else ""
    b = props ^. iconDesc . _1
    c = "_" <> if mouseIsOver && (props ^. iconHighlight)
                 then "RO"
                 else props ^. iconDesc . _2
    d = " " <> "iconsize-" <> map toLower (show (props ^. size))
  div_ ["className" $= fromString ((props ^. blockName) <> "__icon" <> " " <>
                                   (props ^. blockName) <> "__category-icon" <> " " <>
                                   a <> b <> c <> d
                                   )
       , onMouseEnter $ \_ _ _ -> ([], Just True)
       , onMouseLeave $ \_ _ _ -> ([], Just False)
       ] $ do
    span_ ["className" $= "path1"] ""
    span_ ["className" $= "path2"] ""
    span_ ["className" $= "path3"] ""
    span_ ["className" $= "path4"] ""
    span_ ["className" $= "path5"] ""
    span_ ["className" $= "path6"] ""
    span_ ["className" $= "path7"] ""
    span_ ["className" $= "path8"] ""

icon_ :: IconProps -> ReactElementM eventHandler ()
icon_ props = view icon props mempty


iconButtonWithAlignment :: ReactView IconButtonWithAlignmentProps
iconButtonWithAlignment = defineView "IconButtonWithAlignment" $ \props -> do
    let bprops = props ^. iconButtonProps
    let iprops = bprops ^. iconProps
    div_ (["data-content-type" $= (bprops ^. contentType)
           , "style" @= [Style "cursor" ("pointer" :: String)]
           -- TODO unify the naming schema of the classes for the different buttons!
           , "className" $= fromString (concat [ iprops ^. blockName, "__button"
                                               , " "
                                               , iprops ^. blockName
                                               , if bprops ^. elementName == "" then "" else "__"
                                               , bprops ^. elementName
                                               , " "
                                               , iprops ^. blockName
                                               , if bprops ^. elementName == "" then "" else "__"
                                               , bprops ^. elementName
                                               , if bprops ^. moduleName == "" then "" else "--"
                                               , bprops ^. moduleName
                                               , " "
                                               , alignmentClass (iprops ^. blockName)
                                                                (props ^. rightAligned)
                                               ])
           , onClick $ bprops ^. clickHandler
           ] <> case props ^. position of
                   Nothing  -> []
                   Just pos -> ["style" @= [Style "top" pos]]) $ do
        icon_ iprops
        span_ ["className" $= fromString (iprops ^. blockName <> "__button-label")] $
            elemJSString (bprops ^. label)
    where
      alignmentClass blockName_ rightAligned_ = if rightAligned_ then " " <> blockName_ <> "--align-right" else ""

iconButtonWithAlignment_ :: IconButtonWithAlignmentProps -> ReactElementM eventHandler ()
iconButtonWithAlignment_ props = view iconButtonWithAlignment props mempty

iconButton :: ReactView IconButtonProps
iconButton = defineView "IconButton" $ \props ->
    iconButtonWithAlignment_ $ IconButtonWithAlignmentProps props rightAligned_ Nothing
    where rightAligned_ = False -- no right alignment in the standard case

iconButton_ :: IconButtonProps -> ReactElementM eventHandler ()
iconButton_ props = view iconButton props mempty

positionedIconButton :: ReactView (IconButtonProps, Int)
positionedIconButton = defineView "IconButton" $ \(props, position_) ->
    iconButtonWithAlignment_ $ IconButtonWithAlignmentProps props rightAligned_ (Just position_)
    where rightAligned_ = False -- no right alignment in the standard case

positionedIconButton_ :: IconButtonProps -> Int -> ReactElementM eventHandler ()
positionedIconButton_ props position_ = view positionedIconButton (props, position_) mempty
