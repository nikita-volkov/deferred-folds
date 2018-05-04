module DeferredFolds.FoldlMView
where

import DeferredFolds.Prelude hiding (foldl')
import qualified DeferredFolds.Prelude as A


{-|
A monadic variation of "DeferredFolds.FoldlView"
-}
newtype FoldlMView m input =
  FoldlMView (forall output. (output -> input -> m output) -> output -> m output)

deriving instance Functor m => Functor (FoldlMView m)

instance Monad m => Applicative (FoldlMView m) where
  pure x =
    FoldlMView (\ step init -> step init x)
  (<*>) = ap

instance Monad m => Alternative (FoldlMView m) where
  empty =
    FoldlMView (const return)
  {-# INLINE (<|>) #-}
  (<|>) (FoldlMView left) (FoldlMView right) =
    FoldlMView (\ step init -> left step init >>= right step)

instance Monad m => Monad (FoldlMView m) where
  return = pure
  (>>=) (FoldlMView left) rightK =
    FoldlMView $ \ step init ->
    let
      newStep output x =
        case rightK x of
          FoldlMView right ->
            right step output
      in left newStep init

instance Monad m => MonadPlus (FoldlMView m) where
  mzero = empty
  mplus = (<|>)

instance Monad m => Semigroup (FoldlMView m a) where
  (<>) = (<|>)

instance Monad m => Monoid (FoldlMView m a) where
  mempty = empty
  mappend = (<>)

{-| Perform a strict left fold -}
{-# INLINE foldl' #-}
foldl' :: (output -> input -> output) -> output -> FoldlMView Identity input -> output
foldl' step init (FoldlMView run) =
  runIdentity (run identityStep init)
  where
    identityStep state input = return (step state input)

{-| Perform a monadic strict left fold -}
{-# INLINE foldlM' #-}
foldlM' :: Monad m => (output -> input -> m output) -> output -> FoldlMView m input -> m output
foldlM' step init (FoldlMView run) =
  run step init

{-| Apply a Gonzalez fold -}
{-# INLINE fold #-}
fold :: Fold input output -> FoldlMView Identity input -> output
fold (Fold step init extract) = extract . foldl' step init

{-| Apply a monadic Gonzalez fold -}
{-# INLINE foldM #-}
foldM :: Monad m => FoldM m input output -> FoldlMView m input -> m output
foldM (FoldM step init extract) view =
  do
    initialState <- init
    finalState <- foldlM' step initialState view
    extract finalState

{-| Construct from any foldable -}
{-# INLINE foldable #-}
foldable :: (Monad m, Foldable foldable) => foldable a -> FoldlMView m a
foldable foldable = FoldlMView (\ step init -> A.foldlM step init foldable)

{-| Ints in the specified inclusive range -}
intsInRange :: Monad m => Int -> Int -> FoldlMView m Int
intsInRange from to =
  FoldlMView $ \ step init ->
  let
    loop !state int =
      if int <= to
        then do
          newState <- step state int
          loop newState (succ int)
        else return state
    in loop init from
