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

data IconButtonProps = IconButtonProps
    { _blockName     :: String
    , _elementName   :: String
    , _contentType   :: JSString
    , _iconHighlight :: Bool
    , _iconDesc      :: (String, String)
    , _label         :: JSString
    , _size          :: IconSize
    }

makeLenses ''IconButtonProps

data IconButtonWithAlignmentProps = IconButtonWithAlignmentProps
    { _iconButtonProps :: IconButtonProps
    , _rightAligned    :: Bool
    , _position        :: Maybe Int
    }

makeLenses ''IconButtonWithAlignmentProps


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

icon_ :: JSString -> ReactElementM eventHandler ()
icon_ iconClass = view icon iconClass mempty


iconButtonWithAlignment :: ReactView IconButtonWithAlignmentProps
iconButtonWithAlignment = defineStatefulView "IconButtonWithAlignment" False $ \mouseIsOver props -> do
    let bprops = props ^. iconButtonProps
    button_ (["data-content-type" $= (bprops ^. contentType)
           , "className" $= fromString (concat [ bprops ^. blockName, "__button "
                                               , bprops ^. blockName
                                               , if bprops ^. elementName == "" then "" else "__"
                                               , bprops ^. elementName
                                               , alignmentClass (bprops ^. blockName)
                                                                (props ^. rightAligned)
                                               ])
           , onMouseEnter $ \_ _ _ -> ([], Just True)
           , onMouseLeave $ \_ _ _ -> ([], Just False)
           ] <> case props ^. position of
                   Nothing  -> []
                   Just pos -> ["style" @= [Style "top" pos]]) $ do
        div_ ["className" $= fromString ((bprops ^. blockName) <> "__icon")] $ do
            let -- TODO: these could do with better names
                a = if bprops ^. iconHighlight then "o-icon-highlight " else ""
                b = bprops ^. iconDesc . _1
                c = "_" <> if mouseIsOver && (bprops ^. iconHighlight)
                             then "RO"
                             else bprops ^. iconDesc . _2
                d = " " <> "iconsize-" <> map toLower (show (bprops ^. size))
            icon_ $ fromString (a <> b <> c <> d)
        span_ ["className" $= fromString (bprops ^. blockName <> "__button-label")] $
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
