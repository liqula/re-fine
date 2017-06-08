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
module Refine.Prelude.TH (makeRefineType, makeSOPGeneric, makeJSON, makeNFData) where

import Control.Lens (makeLenses, makePrisms)
import Control.DeepSeq (NFData(..))
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
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

typeVarName :: TyVarBndr -> Name
typeVarName (PlainTV name)        = name
typeVarName (KindedTV name _kind) = name

firstTypeParam :: Dec -> Maybe Name
firstTypeParam (DataD _ _ vars _ _ _)    = listToMaybe $ map typeVarName vars
firstTypeParam (NewtypeD _ _ vars _ _ _) = listToMaybe $ map typeVarName vars
firstTypeParam dec = error $ "firstTypeParam got invalid Dec " <> show dec

typeOf :: Name -> Q (Type, Maybe Type)
typeOf t = do
  (TyConI dec) <- reify t
  pure $ case firstTypeParam dec of
    Nothing -> (ConT t, Nothing)
    Just p  -> (AppT (ConT t) (VarT p), Just (VarT p))


makeSOPGeneric :: Name -> Q [Dec]
makeSOPGeneric t = do
  (t', _mtp) <- typeOf t
  pure
    [ mkInstanceD [] (AppT (ConT (''Generic)) t') []
    , mkInstanceD [] (AppT (ConT (''HasDatatypeInfo)) t') []
    ]

makeJSON :: Name -> Q [Dec]
makeJSON t = do
  (VarE toJSONN)        <- [|Data.Aeson.toJSON|]
  (VarE gtoJSONDefN)    <- [|Refine.Prelude.Generic.gtoJSONDef|]
  (VarE parseJSONN)     <- [|Data.Aeson.parseJSON|]
  (VarE gparseJSONDefN) <- [|Refine.Prelude.Generic.gparseJSONDef|]
  let toJSONC   = ConT ''ToJSON
      fromJSONC = ConT ''FromJSON
  (t', mtp) <- typeOf t
  pure
    [ mkInstanceD (maybe [] (\tp -> [AppT toJSONC tp]) mtp) (AppT toJSONC t')
        [ FunD toJSONN [ Clause [] (NormalB (VarE gtoJSONDefN)) []] ]
    , mkInstanceD (maybe [] (\tp -> [AppT fromJSONC tp]) mtp) (AppT fromJSONC t')
        [ FunD parseJSONN [ Clause [] (NormalB (VarE gparseJSONDefN)) []] ]
    ]

makeNFData :: Name -> Q [Dec]
makeNFData t = do
  (VarE rnfN)  <- [|Control.DeepSeq.rnf|]
  (VarE grnfN) <- [|Generics.SOP.NFData.grnf|]
  let nfdataC = ConT ''NFData
  (t', mtp) <- typeOf t
  pure
    [ mkInstanceD (maybe [] (\tp -> [AppT nfdataC tp]) mtp) (AppT (ConT (''NFData)) t')
        [ FunD rnfN [ Clause [] (NormalB (VarE grnfN)) []] ]
    ]

-- FIXME: Support parametric types with more than one parameter.
-- FIXME: rename to makeAllInstances
makeRefineType :: Name -> Q [Dec]
makeRefineType t = do
  l <- makeLenses t
  p <- makePrisms t
  j <- makeJSON t
  n <- makeNFData t
  s <- makeSOPGeneric t
  pure $ concat [l, p, s, j, n]
