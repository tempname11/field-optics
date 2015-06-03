{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
module Binocular where

import Control.Applicative (Const (Const), getConst)
import Data.Functor.Identity (Identity (Identity), runIdentity)

--------------------------------------------------------------------------------

class Binocular (f :: * -> *) (g :: * -> *) | f -> g where
  morph :: f a -> g a
  nothing :: f n -> g a -- `n` is not used.

newtype ConstMay a b = ConstMay { getConstMay :: Const (Maybe a) b }
newtype IdentityMay a = IdentityMay { runIdentityMay :: Maybe a }

deriving instance Functor (ConstMay a)
deriving instance Functor IdentityMay

instance Binocular (Const a) (ConstMay a) where
  morph (Const a) = ConstMay $ Const $ Just a
  nothing = const $ ConstMay $ Const Nothing

instance Binocular Identity IdentityMay where
  morph (Identity a) = IdentityMay $ Just a
  nothing = const $ IdentityMay Nothing

instance Binocular IdentityMay IdentityMay where
  morph = id
  nothing = const $ IdentityMay Nothing

instance Binocular (ConstMay a) (ConstMay a) where
  morph = id
  nothing = const $ ConstMay $ Const Nothing

type Binocle s t a b = forall f g.
               (Functor f, Functor g, Binocular f g) =>
               (a -> f b) -> (s -> g t)

type Binocle' s a = Binocle s s a a

-- set :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
setMay :: ((a -> Identity b) -> s -> IdentityMay t) -> b -> s -> Maybe t
setMay l b s = runIdentityMay $ l (const $ Identity b) s

-- view :: ((a -> Const a a) -> s -> Const a s) -> s -> a
viewMay :: ((a -> Const a a) -> s -> ConstMay a s) -> s -> Maybe a
viewMay l s = getConst $ getConstMay $ l Const s

-- over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
overMay :: ((a -> Identity b) -> s -> IdentityMay t) -> (a -> b) -> s -> Maybe t
overMay l f s = runIdentityMay $ l (Identity . f) s

(^.?) :: s -> ((a -> Const a a) -> s -> ConstMay a s) -> Maybe a
(^.?) = flip viewMay

(%~?) :: ((a -> Identity b) -> s -> IdentityMay t) -> (a -> b) -> s -> Maybe t
(%~?) = overMay

(.~?) :: ((a -> Identity b) -> s -> IdentityMay t) -> b -> s -> Maybe t
(.~?) = setMay
