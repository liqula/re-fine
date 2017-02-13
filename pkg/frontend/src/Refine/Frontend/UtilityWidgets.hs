{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.UtilityWidgets
( IconSize(..)
, IconDescription
, IconProps(..)
, ClickHandler
, IconButtonProps(..)
, IconButtonWithAlignmentProps(IconButtonWithAlignmentProps)

, icon_
, iconButtonWithAlignment_
, iconButtonWithAlignmentCore_
, iconButton_
, positionedIconButton_

, toClasses
) where

import           Control.Lens (makeLenses, (^.), _1, _2)
import           Data.Char (toLower)
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           GHCJS.Types (JSString)
import           React.Flux

import qualified Refine.Frontend.Colors as Color
import           Refine.Frontend.Style
-- TODO import           Refine.Frontend.ThirdPartyViews (hammer_)


data IconSize
    = S
    | M
    | L
    | XL
    | XXL
    deriving Show

type IconDescription = (String, String)

data IconProps = IconProps
  { _blockName     :: String
  , _iconHighlight :: Bool
  , _iconDesc      :: IconDescription
  , _size          :: IconSize

  }

makeLenses ''IconProps

type ClickHandler = Event -> [SomeStoreAction]

data IconButtonProps = IconButtonProps
  { _iconProps     :: IconProps
  , _elementName   :: String
  , _moduleName    :: String
  , _contentType   :: JSString
  , _label         :: JSString
  , _disabled      :: Bool
  , _clickHandler    :: ClickHandler
  }

makeLenses ''IconButtonProps

data IconButtonWithAlignmentProps = IconButtonWithAlignmentProps
    { _iconButtonProps :: IconButtonProps
    , _rightAligned    :: Bool
    , _position        :: Maybe Int
    }

makeLenses ''IconButtonWithAlignmentProps


icon :: ReactView IconProps
icon = defineStatefulView "Icon" False $ \mouseIsOver props -> do
  -- TODO unify the naming schemas of the classes of the different icons!
  let
    highlightStyle = if mouseIsOver && (props ^. iconHighlight)
                     then "RO"
                     else props ^. iconDesc . _2
  div_ ["className" $= (fromString . toClasses)
                         [ (props ^. blockName) <> "__icon"
                         , (props ^. blockName) <> "__category-icon"
                         , if props ^. iconHighlight then "o-icon-highlight" else ""
                         , props ^. iconDesc . _1 <> "_" <> highlightStyle
                         , "iconsize-" <> map toLower (show (props ^. size))
                         ]
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
{- TODO we currently ignore touch device handling because there are some issues with
 - browsers emitting tap events on click and we don't know how to handle these properly.

    let bprops = props ^. iconButtonProps
    hammer_ [on "onTap" $ bprops ^. clickHandler | not (bprops ^. disabled)] $ do
-}
      iconButtonWithAlignmentCore_ props

iconButtonWithAlignment_ :: IconButtonWithAlignmentProps -> ReactElementM eventHandler ()
iconButtonWithAlignment_ props = view iconButtonWithAlignment props mempty

iconButtonWithAlignmentCore :: ReactView IconButtonWithAlignmentProps
iconButtonWithAlignmentCore = defineView "IconButtonWithAlignmentCore" $ \props -> do
    let bprops = props ^. iconButtonProps
    let iprops = bprops ^. iconProps
    let beConnector = if bprops ^. elementName == "" then "" else "__"
    let emConnector = if bprops ^. moduleName == "" then "" else "--"
    let beName  = iprops ^. blockName <> beConnector <> bprops ^. elementName
    let bemName = beName <> emConnector <> bprops ^. moduleName
    div_ ([ "data-content-type" $= (bprops ^. contentType)
           -- TODO unify the naming schema of the classes for the different buttons!
          , "className" $= fromString (toClasses [ iprops ^. blockName <> "__button"
                                                  , beName  -- for the vdoc-toolbar
                                                  , bemName -- for the buttons in the overlays
                                                  , alignmentClass (iprops ^. blockName)
                                                                   (props ^. rightAligned)
                                                   ])
          , "style" @= (case props ^. position of
                               Nothing  -> []
                               Just pos -> [Style "top" pos]
                    <> [Style "cursor" ("pointer" :: String) | not (bprops ^. disabled)])
          ] <> [onClick $ const . (bprops ^. clickHandler) | not (bprops ^. disabled)]
          ) $ do
        icon_ iprops
        span_ ["className" $= fromString (iprops ^. blockName <> "__button-label")
              , "style" @= [Style "color" Color.disabledText | bprops ^. disabled]
              ] $
            elemJSString (bprops ^. label)
    where
      alignmentClass blockName1 rightAligned1 = if rightAligned1 then blockName1 <> "--align-right" else ""

iconButtonWithAlignmentCore_ :: IconButtonWithAlignmentProps -> ReactElementM eventHandler ()
iconButtonWithAlignmentCore_ props = view iconButtonWithAlignmentCore props mempty

iconButton :: ReactView IconButtonProps
iconButton = defineView "IconButton" $ \props ->
    iconButtonWithAlignment_ $ IconButtonWithAlignmentProps props rightAligned1 Nothing
    where rightAligned1 = False -- no right alignment in the standard case

iconButton_ :: IconButtonProps -> ReactElementM eventHandler ()
iconButton_ props = view iconButton props mempty

positionedIconButton :: ReactView (IconButtonProps, Int)
positionedIconButton = defineView "IconButton" $ \(props, position1) ->
    iconButtonWithAlignment_ $ IconButtonWithAlignmentProps props rightAligned1 (Just position1)
    where rightAligned1 = False -- no right alignment in the standard case

positionedIconButton_ :: IconButtonProps -> Int -> ReactElementM eventHandler ()
positionedIconButton_ props position1 = view positionedIconButton (props, position1) mempty


toClasses :: [String] -> String
toClasses = unwords . compact
  where
    compact :: [String] -> [String]
    compact = filter $ not . null
