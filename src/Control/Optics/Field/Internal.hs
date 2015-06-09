{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
module Control.Optics.Field.Internal (
  Binoculars,
  Binoculars',
  FunctorMay,
  nothing,
) where

import Control.Applicative (Const (Const), getConst)

--------------------------------------------------------------------------------

class Functor f => FunctorMay (f :: * -> *) where
  nothing :: f a

instance FunctorMay Maybe where nothing = Nothing
instance FunctorMay (Const (Maybe a)) where nothing = Const $ Nothing

type Binoculars s t a b =
  forall f. FunctorMay f =>
  (a -> f b) -> (s -> f t)

type Binoculars' s a = Binoculars s s a a

