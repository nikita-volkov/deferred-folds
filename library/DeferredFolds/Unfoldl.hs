module DeferredFolds.Unfoldl
where

import DeferredFolds.Prelude
import qualified DeferredFolds.Prelude as A
import qualified DeferredFolds.UnfoldlM as B
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

>newtype Unfoldl a = Unfoldl (forall b. (b -> a -> b) -> b -> b)

Then we get to this simple morphism:

>list :: [a] -> Unfoldl a
>list list = Unfoldl (\ step init -> foldl' step init list)

We can do the same with say "Data.Text.Text":

>text :: Text -> Unfoldl Char
>text text = Unfoldl (\ step init -> Data.Text.foldl' step init text)

And then we can use those both to concatenate with just an @O(1)@ cost:

>abcdef :: Unfoldl Char
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
newtype Unfoldl input =
  Unfoldl (forall output. (output -> input -> output) -> output -> output)

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
unfoldM :: B.UnfoldlM Identity input -> Unfoldl input
unfoldM (B.UnfoldlM runFoldM) = Unfoldl (\ step init -> runIdentity (runFoldM (\ a b -> return (step a b)) init))

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
