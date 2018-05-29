module DeferredFolds.Unfold
where

import DeferredFolds.Prelude
import qualified DeferredFolds.Prelude as A
import qualified Data.Map.Strict as C
import qualified Data.IntMap.Strict as D


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

>newtype Unfold a = Unfold (forall b. (b -> a -> b) -> b -> b)

Then we get to this simple morphism:

>list :: [a] -> Unfold a
>list list = Unfold (\ step init -> foldl' step init list)

We can do the same with say "Data.Text.Text":

>text :: Text -> Unfold Char
>text text = Unfold (\ step init -> Data.Text.foldl' step init text)

And then we can use those both to concatenate with just an @O(1)@ cost:

>abcdef :: Unfold Char
>abcdef = list ['a', 'b', 'c'] <> text "def"

Please notice that up until this moment no actual data materialization has happened and
hence no traversals have appeared.
All that we've done is just composed a function,
which only specifies which parts of data structures to traverse to perform a left-fold.
Only at the moment where the actual folding will happen will we actually traverse the source data.
E.g., using the "fold" function:

>abcdefLength :: Int
>abcdefLength = fold Control.Foldl.length abcdef
-}
newtype Unfold input =
  Unfold (forall output. (output -> input -> output) -> output -> output)

deriving instance Functor Unfold

instance Applicative Unfold where
  pure x =
    Unfold (\ step init -> step init x)
  (<*>) = ap

instance Alternative Unfold where
  empty =
    Unfold (const id)
  {-# INLINE (<|>) #-}
  (<|>) (Unfold left) (Unfold right) =
    Unfold (\ step init -> right step (left step init))

instance Monad Unfold where
  return = pure
  (>>=) (Unfold left) rightK =
    Unfold $ \ step init ->
    let
      newStep output x =
        case rightK x of
          Unfold right ->
            right step output
      in left newStep init

instance MonadPlus Unfold where
  mzero = empty
  mplus = (<|>)

instance Semigroup (Unfold a) where
  (<>) = (<|>)

instance Monoid (Unfold a) where
  mempty = empty
  mappend = (<>)

{-| Perform a strict left fold -}
{-# INLINE foldl' #-}
foldl' :: (output -> input -> output) -> output -> Unfold input -> output
foldl' step init (Unfold run) = run step init

{-| Apply a Gonzalez fold -}
{-# INLINE fold #-}
fold :: Fold input output -> Unfold input -> output
fold (Fold step init extract) (Unfold run) = extract (run step init)

{-| Construct from any foldable -}
{-# INLINE foldable #-}
foldable :: Foldable foldable => foldable a -> Unfold a
foldable foldable = Unfold (\ step init -> A.foldl' step init foldable)

{-| Ints in the specified inclusive range -}
{-# INLINE intsInRange #-}
intsInRange :: Int -> Int -> Unfold Int
intsInRange from to =
  Unfold $ \ step init ->
  let
    loop !state int =
      if int <= to
        then loop (step state int) (succ int)
        else state
    in loop init from

{-# INLINE map #-}
map :: Map key value -> Unfold (key, value)
map map =
  Unfold (\ step init -> C.foldlWithKey' (\ state key value -> step state (key, value)) init map)

{-# INLINE intMap #-}
intMap :: IntMap value -> Unfold (Int, value)
intMap intMap =
  Unfold (\ step init -> D.foldlWithKey' (\ state key value -> step state (key, value)) init intMap)
