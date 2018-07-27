module DeferredFolds.Unfoldl
where

import DeferredFolds.Prelude
import DeferredFolds.Types
import qualified DeferredFolds.Prelude as A
import qualified Data.Map.Strict as C
import qualified Data.IntMap.Strict as D


deriving instance Functor Unfoldl

instance Applicative Unfoldl where
  pure x =
    Unfoldl (\ step init -> step init x)
  (<*>) = ap

instance Alternative Unfoldl where
  empty =
    Unfoldl (const id)
  {-# INLINE (<|>) #-}
  (<|>) (Unfoldl left) (Unfoldl right) =
    Unfoldl (\ step init -> right step (left step init))

instance Monad Unfoldl where
  return = pure
  (>>=) (Unfoldl left) rightK =
    Unfoldl $ \ step init ->
    let
      newStep output x =
        case rightK x of
          Unfoldl right ->
            right step output
      in left newStep init

instance MonadPlus Unfoldl where
  mzero = empty
  mplus = (<|>)

instance Semigroup (Unfoldl a) where
  (<>) = (<|>)

instance Monoid (Unfoldl a) where
  mempty = empty
  mappend = (<>)

instance Foldable Unfoldl where
  {-# INLINE foldMap #-}
  foldMap inputMonoid = foldl' step mempty where
    step monoid input = mappend monoid (inputMonoid input)
  foldl = foldl'
  {-# INLINE foldl' #-}
  foldl' step init (Unfoldl run) = run step init

instance Eq a => Eq (Unfoldl a) where
  (==) left right = toList left == toList right

instance Show a => Show (Unfoldl a) where
  show = show . toList

{-| Apply a Gonzalez fold -}
{-# INLINE fold #-}
fold :: Fold input output -> Unfoldl input -> output
fold (Fold step init extract) (Unfoldl run) = extract (run step init)

{-| Unlift a monadic unfold -}
{-# INLINE unfoldM #-}
unfoldM :: UnfoldlM Identity input -> Unfoldl input
unfoldM (UnfoldlM runFoldM) = Unfoldl (\ step init -> runIdentity (runFoldM (\ a b -> return (step a b)) init))

{-| Construct from any foldable -}
{-# INLINE foldable #-}
foldable :: Foldable foldable => foldable a -> Unfoldl a
foldable foldable = Unfoldl (\ step init -> A.foldl' step init foldable)

{-# INLINE filter #-}
filter :: (a -> Bool) -> Unfoldl a -> Unfoldl a
filter test (Unfoldl run) = Unfoldl (\ step -> run (\ state element -> if test element then step state element else state))

{-| Ints in the specified inclusive range -}
{-# INLINE intsInRange #-}
intsInRange :: Int -> Int -> Unfoldl Int
intsInRange from to =
  Unfoldl $ \ step init ->
  let
    loop !state int =
      if int <= to
        then loop (step state int) (succ int)
        else state
    in loop init from

{-# INLINE map #-}
map :: Map key value -> Unfoldl (key, value)
map map =
  Unfoldl (\ step init -> C.foldlWithKey' (\ state key value -> step state (key, value)) init map)

{-# INLINE intMap #-}
intMap :: IntMap value -> Unfoldl (Int, value)
intMap intMap =
  Unfoldl (\ step init -> D.foldlWithKey' (\ state key value -> step state (key, value)) init intMap)
