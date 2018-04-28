module DeferredFolds.FoldlView
where

import DeferredFolds.Prelude
import qualified DeferredFolds.Prelude as A


{-|
A projection on data, which only knows how to execute a strict left-fold.

It is a monad and a monoid, and is very useful for
efficiently aggregating the projections on data intended for left-folding,
since its concatenation (`<>`) has complexity of @O(1)@.

[Intuition]

The intuition of what this abstraction is all about can be derived from lists.

Let's consider the `Data.List.foldl'` function for lists:

>foldl' :: (b -> a -> b) -> b -> [a] -> b

If we reverse its parameters we get

>foldl' :: [a] -> (b -> a -> b) -> b -> b

Which in Haskell is essentially the same as

>foldl' :: [a] -> (forall b. (b -> a -> b) -> b -> b)

We can isolate that part into an abstraction:

>newtype FoldlView a = FoldlView (forall b. (b -> a -> b) -> b -> b)

Then we get to this simple morphism:

>foldl' :: [a] -> FoldlView a

-}
newtype FoldlView input =
  FoldlView (forall output. (output -> input -> output) -> output -> output)

deriving instance Functor FoldlView

instance Applicative FoldlView where
  pure x =
    FoldlView (\ step init -> step init x)
  (<*>) = ap

instance Alternative FoldlView where
  empty =
    FoldlView (const id)
  {-# INLINE (<|>) #-}
  (<|>) (FoldlView left) (FoldlView right) =
    FoldlView (\ step init -> right step (left step init))

instance Monad FoldlView where
  return = pure
  (>>=) (FoldlView left) rightK =
    FoldlView $ \ step init ->
    let
      newStep output x =
        case rightK x of
          FoldlView right ->
            right step output
      in left newStep init

instance MonadPlus FoldlView where
  mzero = empty
  mplus = (<|>)

instance Semigroup (FoldlView a) where
  (<>) = (<|>)

instance Monoid (FoldlView a) where
  mempty = empty
  mappend = (<>)

{-| Perform a strict left fold -}
{-# INLINE foldl' #-}
foldl' :: (output -> input -> output) -> output -> FoldlView input -> output
foldl' step init (FoldlView run) = run step init

{-| Apply a Gonzalez fold -}
{-# INLINE fold #-}
fold :: Fold input output -> FoldlView input -> output
fold (Fold step init extract) (FoldlView run) = extract (run step init)

{-| Construct from any foldable -}
{-# INLINE foldable #-}
foldable :: Foldable foldable => foldable a -> FoldlView a
foldable foldable = FoldlView (\ step init -> A.foldl' step init foldable)

{-| Ints in the specified inclusive range -}
intsInRange :: Int -> Int -> FoldlView Int
intsInRange from to =
  FoldlView $ \ step init ->
  let
    loop !state int =
      if int <= to
        then loop (step state int) (succ int)
        else state
    in loop init from
