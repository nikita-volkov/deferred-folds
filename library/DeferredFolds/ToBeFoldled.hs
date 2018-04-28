module DeferredFolds.ToBeFoldled
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

>newtype ToBeFoldled a = ToBeFoldled (forall b. (b -> a -> b) -> b -> b)

Then we get to this simple morphism:

>foldl' :: [a] -> ToBeFoldled a

-}
newtype ToBeFoldled input =
  ToBeFoldled (forall output. (output -> input -> output) -> output -> output)

deriving instance Functor ToBeFoldled

instance Applicative ToBeFoldled where
  pure x =
    ToBeFoldled (\ step init -> step init x)
  (<*>) = ap

instance Alternative ToBeFoldled where
  empty =
    ToBeFoldled (const id)
  {-# INLINE (<|>) #-}
  (<|>) (ToBeFoldled left) (ToBeFoldled right) =
    ToBeFoldled (\ step init -> right step (left step init))

instance Monad ToBeFoldled where
  return = pure
  (>>=) (ToBeFoldled left) rightK =
    ToBeFoldled $ \ step init ->
    let
      newStep output x =
        case rightK x of
          ToBeFoldled right ->
            right step output
      in left newStep init

instance MonadPlus ToBeFoldled where
  mzero = empty
  mplus = (<|>)

instance Semigroup (ToBeFoldled a) where
  (<>) = (<|>)

instance Monoid (ToBeFoldled a) where
  mempty = empty
  mappend = (<>)

{-| Perform a strict left fold -}
{-# INLINE foldl' #-}
foldl' :: (output -> input -> output) -> output -> ToBeFoldled input -> output
foldl' step init (ToBeFoldled run) = run step init

{-| Apply a Gonzalez fold -}
{-# INLINE fold #-}
fold :: Fold input output -> ToBeFoldled input -> output
fold (Fold step init extract) (ToBeFoldled run) = extract (run step init)

{-| Construct from any foldable -}
{-# INLINE foldable #-}
foldable :: Foldable foldable => foldable a -> ToBeFoldled a
foldable foldable = ToBeFoldled (\ step init -> A.foldl' step init foldable)
