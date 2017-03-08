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

import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Types as RS
import           Refine.Frontend.UtilityWidgets

editToolbar :: ReactView ()
editToolbar = defineView "EditToolbar" $ \() ->
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
                      (\_ -> RS.dispatch RS.ShowNotImplementedYet)
                      []
          iconButton_ $ IconButtonProps
                      (IconProps "c-vdoc-toolbar" True ("icon-", "dark") XXL)
                      "btn-index"
                      ""
                      ""
                      "header 2"
                      False
                      (\_ -> RS.dispatch RS.ShowNotImplementedYet)
                      []
          iconButton_ $ IconButtonProps
                      (IconProps "c-vdoc-toolbar" True ("icon-", "dark") XXL)
                      "btn-index"
                      ""
                      ""
                      "header 3"
                      False
                      (\_ -> RS.dispatch RS.ShowNotImplementedYet)
                      []

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ IconButtonProps
                      (IconProps "c-vdoc-toolbar" True ("icon-", "dark") XXL)
                      "btn-index"
                      ""
                      ""
                      "bold"
                      False
                      (\_ -> RS.dispatch RS.ShowNotImplementedYet)
                      []
          iconButton_ $ IconButtonProps
                      (IconProps "c-vdoc-toolbar" True ("icon-", "dark") XXL)
                      "btn-index"
                      ""
                      ""
                      "italic"
                      False
                      (\_ -> RS.dispatch RS.ShowNotImplementedYet)
                      []

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ IconButtonProps
                      (IconProps "c-vdoc-toolbar" True ("icon-", "dark") XXL)
                      "btn-index"
                      ""
                      ""
                      "bullets"
                      False
                      (\_ -> RS.dispatch RS.ShowNotImplementedYet)
                      []
          iconButton_ $ IconButtonProps
                      (IconProps "c-vdoc-toolbar" True ("icon-", "dark") XXL)
                      "btn-index"
                      ""
                      ""
                      "numbers"
                      False
                      (\_ -> RS.dispatch RS.ShowNotImplementedYet)
                      -- (\e -> stopPropagation e : RS.dispatch (RS.HeaderAction RS.ToggleEditToolbarExtension))
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
                (\_ -> RS.dispatch RS.ShowNotImplementedYet)
                [])
              True
              Nothing

editToolbar_ :: ReactElementM eventHandler ()
editToolbar_ = view editToolbar () mempty



