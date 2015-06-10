{-# LANGUAGE TemplateHaskell #-}
module Extra.Field.Optics.TH (
  makeFieldOptics,
  makeFieldOpticsWith,
) where

import Control.Lens (Lens')
import Extra.Field.Optics.Internal (nothing, Binoculars')
import Language.Haskell.TH hiding (appE, appT)
import Data.Traversable (for)
import Debug.Trace

{-
data N = N
data M = M
data A = A1 { a'n :: N } | A2 { a'm :: M }

a'n_ :: Binoculars' A N
a'n_ f s = case s of
  A1 a -> fmap (\z -> s { a'n = z }) (f a)
  _ -> nothing
-}

--debug x = traceShow (lines $ pprint x) x

makeFieldOpticsWith :: (String -> Maybe String) -> Name -> Q [Dec]
makeFieldOpticsWith opticNaming tname = do
  let renamed = fmap mkName . opticNaming . nameBase
  let failNamed = fail . (++ nameBase tname) . (++ " when making optics for ")
  info <- reify tname
  cs <- case info of
    TyConI dec -> case dec of
      DataD _cxt _tn _tvs cs _der -> return cs
      _ -> failNamed "Expected a data declaration"
    _ -> failNamed "Expected a type constructor"
  let c = length cs > 1
  let s = ConT tname
  return $
    concat . concat $
      flip fmap cs $ \con ->
        case con of
          RecC cname vs ->
            let total = length vs
            in flip fmap (zip vs [1..total]) $ \((fname,_,a),ours) ->
              case renamed fname of
                Nothing -> []
                Just lname ->
                  decsOf cname fname lname total ours s a c
          _ -> []

decsOf :: Name -> Name -> Name -> Int -> Int -> Type -> Type -> Bool -> [Dec]
decsOf cname fname lname total ours st at may = [dec1, dec2]
  where
  ct = ConT $ if may then ''Binoculars' else ''Lens'
  dec1 = SigD lname (ct `AppT` st `AppT` at)
  dec2 = FunD lname [Clause pats body []]
  pats = [VarP f, VarP s]
  body = NormalB (CaseE (VarE s) matches)
  matches = if may then [match1, match2] else [match1]
  match1 = Match (ConP cname (map patFrom [1..total])) body1 []
  match2 = Match WildP body2 []
  patFrom n = if n == ours then VarP a else WildP
  body1 = NormalB ((VarE 'fmap) `AppE` lambda `AppE` fa)
  body2 = NormalB (VarE 'nothing)
  lambda = LamE [VarP z] $ RecUpdE (VarE s) [(fname, VarE z)]
  a = mkName "a"
  s = mkName "s"
  z = mkName "z"
  f = mkName "f"
  fa = VarE f `AppE` VarE a

makeFieldOptics :: Name -> Q [Dec]
makeFieldOptics = makeFieldOpticsWith defaultOpticNaming

defaultOpticNaming :: String -> Maybe String
defaultOpticNaming s = Just $ s ++ "_"
