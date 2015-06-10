{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
module Example where

import Language.Haskell.TH
import Extra.Field.Optics
import Extra.Field.Optics.TH
import Extra.Field.Optics.Internal

data A = A1 | A2
data B = B

data P = P { -- Product
  p1 :: A,
  p2 :: B
}

data S -- Sum
  = S1 { s1 :: A }
  | S2 { s2 :: B }
  | S3 { s3 :: P }

data M -- Mixed
  = M0
  | M1 { m1  :: S }
  | M2 { m21 :: A ,
         m22 :: B }

deriving instance Show A
deriving instance Show B
deriving instance Show P
deriving instance Show S
deriving instance Show M

makeFieldOptics ''P
makeFieldOptics ''S
makeFieldOptics ''M

rotateA :: A -> A
rotateA A1 = A2
rotateA A2 = A1

