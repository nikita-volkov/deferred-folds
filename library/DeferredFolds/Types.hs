module DeferredFolds.Types
where

import DeferredFolds.Prelude


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
newtype Unfoldl a = Unfoldl (forall x. (x -> a -> x) -> x -> x)

{-|
A monadic variation of "DeferredFolds.Unfoldl"
-}
newtype UnfoldlM m a = UnfoldlM (forall x. (x -> a -> m x) -> x -> m x)

newtype Unfoldr a = Unfoldr (forall x. (a -> x -> x) -> x -> x)

newtype UnfoldrM m a = UnfoldrM (forall x. (a -> x -> m x) -> x -> m x)
