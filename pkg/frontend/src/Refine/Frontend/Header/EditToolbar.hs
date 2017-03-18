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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Header.EditToolbar where

import           React.Flux

import           Refine.Frontend.Store
import           Refine.Frontend.Types
import           Refine.Frontend.Document.Types
import           Refine.Frontend.UtilityWidgets

editToolbar :: View '[()]  -- TODO: can we get rid of the ()?  (grep for '[()]', it also happens in one more case!)
editToolbar = mkView "EditToolbar" $ \() ->
  header_ ["className" $= "row row-align-middle c-vdoc-toolbar"] $ do
    div_ ["className" $= "grid-wrapper"] $ do
      div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
        div_ ["className" $= "c-vdoc-toolbar__content"] $ do

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ IconButtonProps
                      (IconProps "c-vdoc-toolbar" True ("icon-", "dark") XXL)
                      "btn-index"
                      ""
                      ""
                      "header 1"
                      False
                      (\_ -> dispatch ShowNotImplementedYet)
                      []
          iconButton_ $ IconButtonProps
                      (IconProps "c-vdoc-toolbar" True ("icon-", "dark") XXL)
                      "btn-index"
                      ""
                      ""
                      "header 2"
                      False
                      (\_ -> dispatch ShowNotImplementedYet)
                      []
          iconButton_ $ IconButtonProps
                      (IconProps "c-vdoc-toolbar" True ("icon-", "dark") XXL)
                      "btn-index"
                      ""
                      ""
                      "header 3"
                      False
                      (\_ -> dispatch ShowNotImplementedYet)
                      []

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ IconButtonProps
                      (IconProps "c-vdoc-toolbar" True ("icon-", "dark") XXL)
                      "btn-index"
                      ""
                      ""
                      "bold"
                      False
                      (\_ -> dispatch ShowNotImplementedYet)
                      []
          iconButton_ $ IconButtonProps
                      (IconProps "c-vdoc-toolbar" True ("icon-", "dark") XXL)
                      "btn-index"
                      ""
                      ""
                      "italic"
                      False
                      (\_ -> dispatch ShowNotImplementedYet)
                      []

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ IconButtonProps
                      (IconProps "c-vdoc-toolbar" True ("icon-", "dark") XXL)
                      "btn-index"
                      ""
                      ""
                      "bullets"
                      False
                      (\_ -> dispatch ShowNotImplementedYet)
                      []
          iconButton_ $ IconButtonProps
                      (IconProps "c-vdoc-toolbar" True ("icon-", "dark") XXL)
                      "btn-index"
                      ""
                      ""
                      "numbers"
                      False
                      (\_ -> dispatch ShowNotImplementedYet)
                      -- (\e -> stopPropagation e : dispatch (HeaderAction ToggleEditToolbarExtension))
                      []

          iconButtonWithAlignment_ $
            IconButtonWithAlignmentProps
              (IconButtonProps
                (IconProps "c-vdoc-toolbar" True ("icon-Save", "bright") XXL)
                "btn-index"
                ""
                ""
                "save"
                False
                (\_ -> dispatch $ DocumentAction DocumentEditSave)
                [])
              True
              Nothing

editToolbar_ :: ReactElementM eventHandler ()
editToolbar_ = view_ editToolbar "editToolbar_" ()
