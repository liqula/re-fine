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
module Refine.Prelude.TH (makeRefineType, makeRefineTypes, makeRefineType', makeSOPGeneric, makeJSON, makeNFData) where

import Control.Lens (makeLenses, makePrisms)
import Control.DeepSeq (NFData(..))
import Data.Aeson (FromJSON(..), ToJSON(..))
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

typeParams :: Dec -> [Name]
typeParams (DataD _ _ vars _ _ _)    = map typeVarName vars
typeParams (NewtypeD _ _ vars _ _ _) = map typeVarName vars
typeParams dec = error $ "typeParams got invalid Dec " <> show dec

unApp :: Type -> (Type, [Type])
unApp (AppT x y) = (++[y]) <$> unApp x
unApp n = (n, [])

typeOf :: Type -> Q (Type, [Type])
typeOf t_ = do
  let (ConT t, vs) = unApp t_
  TyConI dec <- reify t
  let tps = VarT <$> drop (length vs) (typeParams dec)
  pure (foldl AppT t_ tps, tps)

makeSOPGeneric :: Name -> Q [Dec]
makeSOPGeneric = makeSOPGeneric' . ConT

makeSOPGeneric' :: Type -> Q [Dec]
makeSOPGeneric' t = do
  (t', _mtp) <- typeOf t
  pure
    [ mkInstanceD [] (AppT (ConT (''Generic)) t') []
    , mkInstanceD [] (AppT (ConT (''HasDatatypeInfo)) t') []
    ]

makeJSON :: Name -> Q [Dec]
makeJSON = makeJSON' . ConT

makeJSON' :: Type -> Q [Dec]
makeJSON' t = do
  (VarE toJSONN)        <- [|Data.Aeson.toJSON|]
  (VarE gtoJSONDefN)    <- [|Refine.Prelude.Generic.gtoJSONDef|]
  (VarE parseJSONN)     <- [|Data.Aeson.parseJSON|]
  (VarE gparseJSONDefN) <- [|Refine.Prelude.Generic.gparseJSONDef|]
  let toJSONC   = ConT ''ToJSON
      fromJSONC = ConT ''FromJSON
  (t', mtp) <- typeOf t
  pure
    [ mkInstanceD (AppT toJSONC <$> mtp) (AppT toJSONC t')
        [ FunD toJSONN [ Clause [] (NormalB (VarE gtoJSONDefN)) []] ]
    , mkInstanceD (AppT fromJSONC <$> mtp) (AppT fromJSONC t')
        [ FunD parseJSONN [ Clause [] (NormalB (VarE gparseJSONDefN)) []] ]
    ]

makeNFData :: Name -> Q [Dec]
makeNFData = makeNFData' . ConT

makeNFData' :: Type -> Q [Dec]
makeNFData' t = do
  (VarE rnfN)  <- [|Control.DeepSeq.rnf|]
  (VarE grnfN) <- [|Generics.SOP.NFData.grnf|]
  let nfdataC = ConT ''NFData
  (t', mtp) <- typeOf t
  pure
    [ mkInstanceD (AppT nfdataC <$> mtp) (AppT (ConT (''NFData)) t')
        [ FunD rnfN [ Clause [] (NormalB (VarE grnfN)) []] ]
    ]

-- FIXME: rename to makeInstances
makeRefineType :: Name -> Q [Dec]
makeRefineType t = do
  l <- makeLenses t
  p <- makePrisms t
  j <- makeJSON t
  n <- makeNFData t
  s <- makeSOPGeneric t
  pure $ concat [l, p, s, j, n]

makeRefineTypes :: [Name] -> Q [Dec]
makeRefineTypes ns = concat <$> mapM makeRefineType ns

-- FIXME: rename to makeInstances'
makeRefineType' :: Q Type -> Q [Dec]
makeRefineType' qt = do
  t <- qt
  j <- makeJSON' t
  n <- makeNFData' t
  s <- makeSOPGeneric' t
  pure $ concat [s, j, n]
