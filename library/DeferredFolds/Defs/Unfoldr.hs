module DeferredFolds.Defs.Unfoldr
where

import DeferredFolds.Prelude hiding (fold, reverse)
import DeferredFolds.Types
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Short.Internal as ShortByteString


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
  {-# INLINE foldr #-}
  foldr step state (Unfoldr run) = run step state
  foldl = foldl'
  {-# INLINE foldl' #-}
  foldl' leftStep state (Unfoldr unfoldr) = unfoldr rightStep id state where
    rightStep element k state = k $! leftStep state element

instance Eq a => Eq (Unfoldr a) where
  (==) left right = toList left == toList right

instance Show a => Show (Unfoldr a) where
  show = show . toList

{-| Apply a Gonzalez fold -}
{-# INLINE fold #-}
fold :: Fold input output -> Unfoldr input -> output
fold (Fold step init extract) = extract . foldl' step init

{-| Construct from any foldable -}
{-# INLINE foldable #-}
foldable :: Foldable foldable => foldable a -> Unfoldr a
foldable foldable = Unfoldr (\ step init -> foldr step init foldable)

{-| Filter the values given a predicate -}
{-# INLINE filter #-}
filter :: (a -> Bool) -> Unfoldr a -> Unfoldr a
filter test (Unfoldr run) = Unfoldr (\ step -> run (\ element state -> if test element then step element state else state))

{-| Ascending infinite stream of ints starting from the one specified -}
{-# INLINE intsFrom #-}
intsFrom :: Int -> Unfoldr Int
intsFrom from = Unfoldr $ \ step init -> let
  loop int = step int (loop (succ int))
  in loop from

{-| Ints in the specified inclusive range -}
{-# INLINE intsInRange #-}
intsInRange :: Int -> Int -> Unfoldr Int
intsInRange from to =
  Unfoldr $ \ step init ->
  let
    loop int =
      if int <= to
        then step int (loop (succ int))
        else init
    in loop from

{-| Associations of a map -}
{-# INLINE mapAssocs #-}
mapAssocs :: Map key value -> Unfoldr (key, value)
mapAssocs map =
  Unfoldr (\ step init -> Map.foldrWithKey (\ key value state -> step (key, value) state) init map)

{-| Associations of an intmap -}
{-# INLINE intMapAssocs #-}
intMapAssocs :: IntMap value -> Unfoldr (Int, value)
intMapAssocs intMap =
  Unfoldr (\ step init -> IntMap.foldrWithKey (\ key value state -> step (key, value) state) init intMap)

{-| Bytes of a bytestring -}
{-# INLINE byteStringBytes #-}
byteStringBytes :: ByteString -> Unfoldr Word8
byteStringBytes bs = Unfoldr (\ step init -> ByteString.foldr step init bs)

{-| Bytes of a short bytestring -}
{-# INLINE shortByteStringBytes #-}
shortByteStringBytes :: ShortByteString -> Unfoldr Word8
shortByteStringBytes (ShortByteString.SBS ba#) = primArray (PrimArray ba#)

{-| Elements of a prim array -}
{-# INLINE primArray #-}
primArray :: (Prim prim) => PrimArray prim -> Unfoldr prim
primArray ba = Unfoldr $ \ f z -> foldrPrimArray f z ba

{-| Elements of a prim array coming paired with indices -}
{-# INLINE primArrayWithIndices #-}
primArrayWithIndices :: (Prim prim) => PrimArray prim -> Unfoldr (Int, prim)
primArrayWithIndices pa = Unfoldr $ \ step state -> let
  !size = sizeofPrimArray pa
  loop index = if index < size
    then step (index, indexPrimArray pa index) (loop (succ index))
    else state
  in loop 0

{-|
Extract individual digits of a non-negative integral number.
-}
decimalDigits :: Integral a => a -> Unfoldr a
decimalDigits = reverse . reverseDecimalDigits

{-|
Extract individual digits of a non-negative integral number in reverse order.
More efficient than 'digits'.
-}
reverseDecimalDigits :: Integral a => a -> Unfoldr a
reverseDecimalDigits = reverseDigits 10

hexadecimalDigits :: Integral a => a -> Unfoldr a
hexadecimalDigits = reverse . reverseHexadecimalDigits

reverseHexadecimalDigits :: Integral a => a -> Unfoldr a
reverseHexadecimalDigits = reverseDigits 16

reverseDigits :: Integral a => a -> a -> Unfoldr a
reverseDigits radix x = Unfoldr $ \ step init -> let
  loop x = case divMod x radix of
    (next, digit) -> step digit (if next <= 0 then init else loop next)
  in loop x

{-|
Reverse the order

Use with care, because it requires to allocate all elements.
-}
reverse :: Unfoldr a -> Unfoldr a
reverse (Unfoldr unfoldr) = Unfoldr $ \ step -> unfoldr (\ a f -> f . step a) id

{-|
Lift into an unfold, which produces pairs with right-associative index.
-}
zipWithReverseIndex :: Unfoldr a -> Unfoldr (Int, a)
zipWithReverseIndex (Unfoldr unfoldr) = Unfoldr $ \ step init -> snd $ unfoldr
  (\ a (index, state) -> (succ index, step (index, a) state))
  (0, init)
