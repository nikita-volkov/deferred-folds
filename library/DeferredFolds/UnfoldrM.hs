module DeferredFolds.UnfoldrM
where

import DeferredFolds.Prelude
import DeferredFolds.Unfoldr (Unfoldr(..))


newtype UnfoldrM m a = UnfoldrM (forall x. (a -> x -> m x) -> x -> m x)

unfoldr :: Monad m => Unfoldr a -> UnfoldrM m a
unfoldr (Unfoldr unfoldr) = UnfoldrM $ \ stepM -> let
  step input act state = stepM input state >>= act
  in unfoldr step return
