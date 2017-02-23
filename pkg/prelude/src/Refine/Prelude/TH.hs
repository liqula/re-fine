{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

-- FIXME: rename to Refine.Prelude.MakeInstances
module Refine.Prelude.TH where

import Control.Lens (makeLenses, makePrisms)
import Control.DeepSeq (NFData(..))
import Data.Aeson (FromJSON(..), ToJSON(..))
import Language.Haskell.TH
import Generics.SOP
import Generics.SOP.NFData (grnf)

import Refine.Prelude.Generic



mkInstanceD :: Cxt -> Type -> [Dec] -> Dec
#if __GLASGOW_HASKELL__ >= 800
mkInstanceD = InstanceD Nothing
#else
mkInstanceD = InstanceD
#endif


makeSOPGeneric :: Name -> Q [Dec]
makeSOPGeneric t = pure
  [ mkInstanceD [] (AppT (ConT (''Generic)) (ConT t)) []
  , mkInstanceD [] (AppT (ConT (''HasDatatypeInfo)) (ConT t)) []
  ]

makeJSON :: Name -> Q [Dec]
makeJSON t = do
  (VarE toJSONN)        <- [|Data.Aeson.toJSON|]
  (VarE gtoJSONDefN)    <- [|Refine.Prelude.Generic.gtoJSONDef|]
  (VarE parseJSONN)     <- [|Data.Aeson.parseJSON|]
  (VarE gparseJSONDefN) <- [|Refine.Prelude.Generic.gparseJSONDef|]
  pure
    [ mkInstanceD [] (AppT (ConT (''ToJSON)) (ConT t))
        [ FunD toJSONN [ Clause [] (NormalB (VarE gtoJSONDefN)) []] ]
    , mkInstanceD [] (AppT (ConT (''FromJSON)) (ConT t))
        [ FunD parseJSONN [ Clause [] (NormalB (VarE gparseJSONDefN)) []] ]
    ]

makeNFData :: Name -> Q [Dec]
makeNFData t = do
  (VarE rnfN)  <- [|Control.DeepSeq.rnf|]
  (VarE grnfN) <- [|Generics.SOP.NFData.grnf|]
  pure
    [ mkInstanceD [] (AppT (ConT (''NFData)) (ConT t))
        [ FunD rnfN [ Clause [] (NormalB (VarE grnfN)) []] ]
    ]

-- FIXME: Support parametric types
-- FIXME: rename to makeInstances
makeRefineType :: Name -> Q [Dec]
makeRefineType t = do
  l <- makeLenses t
  p <- makePrisms t
  s <- makeSOPGeneric t
  j <- makeJSON t
  n <- makeNFData t
  pure $ concat [l, p, s, j, n]
