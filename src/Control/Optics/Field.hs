module Control.Optics.Field (
  setMay,
  viewMay,
  overMay,
  (^.?),
  (%~?),
  (.~?),
) where

import Control.Applicative (Const (Const), getConst)

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

(^.?) = flip viewMay
(%~?) = overMay
(.~?) = setMay
