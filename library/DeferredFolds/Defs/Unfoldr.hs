module DeferredFolds.Defs.Unfoldr
where

import DeferredFolds.Prelude
import DeferredFolds.Types


deriving instance Functor Unfoldr

instance Applicative Unfoldr where
  pure x = Unfoldr (\ step -> step x)
  (<*>) = ap

instance Alternative Unfoldr where
  empty = Unfoldr (const id)
  {-# INLINE (<|>) #-}
  (<|>) (Unfoldr left) (Unfoldr right) = Unfoldr (\ step init -> left step (right step init))

instance Monad Unfoldr where
  return = pure
  {-# INLINE (>>=) #-}
  (>>=) (Unfoldr left) rightK =
    Unfoldr $ \ step -> left $ \ input -> case rightK input of Unfoldr right -> right step

instance MonadPlus Unfoldr where
  mzero = empty
  mplus = (<|>)

instance Semigroup (Unfoldr a) where
  (<>) = (<|>)

instance Monoid (Unfoldr a) where
  mempty = empty
  mappend = (<>)

instance Foldable Unfoldr where
  {-# INLINE foldMap #-}
  foldMap fn (Unfoldr unfoldr) = unfoldr (mappend . fn) mempty
  foldl = foldl'
  {-# INLINE foldl' #-}
  foldl' leftStep state (Unfoldr unfoldr) = unfoldr rightStep id state where
    rightStep element k state = k $! leftStep state element

instance Eq a => Eq (Unfoldr a) where
  (==) left right = toList left == toList right

instance Show a => Show (Unfoldr a) where
  show = show . toList
