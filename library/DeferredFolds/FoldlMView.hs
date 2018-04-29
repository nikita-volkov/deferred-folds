module DeferredFolds.FoldlMView
where

import DeferredFolds.Prelude hiding (foldl')
import qualified DeferredFolds.Prelude as A


newtype FoldlMView input =
  FoldlMView (forall m output. Monad m => (output -> input -> m output) -> output -> m output)

deriving instance Functor FoldlMView

instance Applicative FoldlMView where
  pure x =
    FoldlMView (\ step init -> step init x)
  (<*>) = ap

instance Alternative FoldlMView where
  empty =
    FoldlMView (const return)
  {-# INLINE (<|>) #-}
  (<|>) (FoldlMView left) (FoldlMView right) =
    FoldlMView (\ step init -> left step init >>= right step)

instance Monad FoldlMView where
  return = pure
  (>>=) (FoldlMView left) rightK =
    FoldlMView $ \ step init ->
    let
      newStep output x =
        case rightK x of
          FoldlMView right ->
            right step output
      in left newStep init

instance MonadPlus FoldlMView where
  mzero = empty
  mplus = (<|>)

instance Semigroup (FoldlMView a) where
  (<>) = (<|>)

instance Monoid (FoldlMView a) where
  mempty = empty
  mappend = (<>)

{-| Perform a strict left fold -}
{-# INLINE foldl' #-}
foldl' :: (output -> input -> output) -> output -> FoldlMView input -> output
foldl' step init (FoldlMView run) =
  runIdentity (run identityStep init)
  where
    identityStep state input = return (step state input)

{-| Apply a Gonzalez fold -}
{-# INLINE fold #-}
fold :: Fold input output -> FoldlMView input -> output
fold (Fold step init extract) = extract . foldl' step init

{-| Construct from any foldable -}
{-# INLINE foldable #-}
foldable :: Foldable foldable => foldable a -> FoldlMView a
foldable foldable = FoldlMView (\ step init -> A.foldlM step init foldable)

{-| Ints in the specified inclusive range -}
intsInRange :: Int -> Int -> FoldlMView Int
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
