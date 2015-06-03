module Monocular where

{-
-- class Monocular (f :: * -> *) (g :: * -> *)
-- instance Monocular (Const a) (MConst a)
-- instance Monocular (MConst a) (MConst a)
-- instance Monocular (ConstM a) (ConstM a)
-- instance Monocular (Const a) (ConstM a)
-- instance Monocular Identity Identity
type Monocle s t a b = forall f g.
               (Functor f, Functor g, Monocular f g) =>
               (a -> f b) -> (s -> g t)
type Monocle' s a = Monocle s s a a
-}
