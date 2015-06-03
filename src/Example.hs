{-# LANGUAGE StandaloneDeriving #-}
module Example where

import Binocular
import Control.Lens

data A = A1 | A2
data B = B
data T = T1 A | T2 B | T3 (A, B)
data Z = Z0 | Z1 T
data S = S A

deriving instance Show A
deriving instance Show B
deriving instance Show T
deriving instance Show Z
deriving instance Show S

rotateA :: A -> A
rotateA A1 = A2
rotateA A2 = A1

s :: S
s = S A1

t11 :: T
t11 = T1 A1

t2 :: T
t2 = T2 B

t31 :: T
t31 = T3 (A1, B)

z131 :: Z
z131 = Z1 t31

t1_ :: Binocle' T A
t1_ f t = case t of
  T1 a -> morph $ T1 <$> f a
  _ -> nothing $ f undefined

t2_ :: Binocle' T B
t2_ f t = case t of
  T2 b -> morph $ T2 <$> f b
  _ -> nothing $ f undefined

t3_ :: Binocle' T (A, B)
t3_ f t = case t of
  T3 x -> morph $ T3 <$> f x
  _ -> nothing $ f undefined

z0_ :: Binocle' Z ()
z0_ f z = case z of
  Z0 -> morph $ const Z0 <$> f ()
  _ -> nothing $ f undefined

z1_ :: Binocle' Z T
z1_ f z = case z of
  Z1 t -> morph $ Z1 <$> f t
  _ -> nothing $ f undefined

