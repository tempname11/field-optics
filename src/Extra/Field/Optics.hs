{-# LANGUAGE Rank2Types #-}
module Extra.Field.Optics (
  setMay,
  viewMay,
  overMay,
  (^.?),
  (%~?),
  (.~?),
  by,
  useMay,
) where

import Control.Applicative (Const (Const), getConst)
import qualified Data.Map as M
import Extra.Field.Optics.Internal
import Control.Monad.State (MonadState, gets)

--------------------------------------------------------------------------------

-- set :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
setMay :: ((a -> Maybe b) -> s -> Maybe t) -> b -> s -> Maybe t
setMay l b s = l (const $ Just b) s

-- view :: ((a -> Const a a) -> s -> Const a s) -> s -> a
viewMay :: ((a -> Const (Maybe a) a) -> s -> Const (Maybe a) s) -> s -> Maybe a
viewMay l s = getConst $ l (Const . Just) s

-- over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
overMay :: ((a -> Maybe b) -> s -> Maybe t) -> (a -> b) -> s -> Maybe t
overMay l f s = l (Just . f) s

useMay :: MonadState s m =>
          ((a -> Const (Maybe a) a) -> s -> Const (Maybe a) s) -> m (Maybe a)
useMay l = gets (viewMay l)

(^.?) = flip viewMay
(%~?) = overMay
(.~?) = setMay

by :: Ord k => k -> Binoculars' (M.Map k v) v
by k f m = case M.lookup k m of
  Just v -> fmap (\z -> M.insert k z m) (f v)
  _ -> nothing
