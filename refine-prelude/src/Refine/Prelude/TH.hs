{-# LANGUAGE FlexibleContexts #-}
module Refine.Prelude.TH where

import Control.Lens (makeLenses, makePrisms)
import Control.DeepSeq
import Language.Haskell.TH
import Generics.SOP        as SOP
import Generics.SOP.JSON   as SOP
import Generics.SOP.NFData

import Refine.Prelude.Generic



makeSOPGeneric :: Name -> Q [Dec]
makeSOPGeneric t = pure
  [ InstanceD Nothing [] (AppT (ConT (''SOP.Generic)) (ConT t)) []
  , InstanceD Nothing [] (AppT (ConT (''SOP.HasDatatypeInfo)) (ConT t)) []
  ]

makeJSON :: Name -> Q [Dec]
makeJSON t = do
  (VarE toJSONN)        <- [|toJSON|]
  (VarE gtoJSONDefN)    <- [|gtoJSONDef|]
  (VarE parseJSONN)     <- [|parseJSON|]
  (VarE gparseJSONDefN) <- [|gparseJSONDef|]
  pure
    [ InstanceD Nothing [] (AppT (ConT (''ToJSON)) (ConT t))
        [ FunD toJSONN [ Clause [] (NormalB (VarE gtoJSONDefN)) []] ]
    , InstanceD Nothing [] (AppT (ConT (''FromJSON)) (ConT t))
        [ FunD parseJSONN [ Clause [] (NormalB (VarE gparseJSONDefN)) []] ]
    ]

makeNFData :: Name -> Q [Dec]
makeNFData t = do
  (VarE rnfN)  <- [|rnf|]
  (VarE grnfN) <- [|grnf|]
  pure
    [ InstanceD Nothing [] (AppT (ConT (''NFData)) (ConT t))
        [ FunD rnfN [ Clause [] (NormalB (VarE grnfN)) []] ]
    ]

-- FIXME: Support parametric types
makeRefineType :: Name -> Q [Dec]
makeRefineType t = do
  l <- makeLenses t
  p <- makePrisms t
  s <- makeSOPGeneric t
  j <- makeJSON t
  n <- makeNFData t
  pure $ concat [l, p, s, j, n]