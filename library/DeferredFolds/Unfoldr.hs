module DeferredFolds.Unfoldr
where

import DeferredFolds.Prelude


newtype Unfoldr a = Unfoldr (forall x. (a -> x -> x) -> x -> x)

foldl' :: (state -> element -> state) -> state -> Unfoldr element -> state
foldl' leftStep state (Unfoldr unfoldr) = unfoldr rightStep id state where
  rightStep element k state = k $! leftStep state element
