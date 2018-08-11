module DeferredFolds.UnfoldM
where

import DeferredFolds.Prelude hiding (mapM_, foldM)
import qualified DeferredFolds.Prelude as A
import qualified Data.ByteString.Internal as ByteString
import qualified Data.ByteString.Short.Internal as ShortByteString


{-|
A monadic variation of "DeferredFolds.Unfold"
-}
newtype UnfoldM m input =
  UnfoldM (forall output. (output -> input -> m output) -> output -> m output)

deriving instance Functor m => Functor (UnfoldM m)

instance Monad m => Applicative (UnfoldM m) where
  pure x =
    UnfoldM (\ step init -> step init x)
  (<*>) = ap

instance Monad m => Alternative (UnfoldM m) where
  empty =
    UnfoldM (const return)
  {-# INLINE (<|>) #-}
  (<|>) (UnfoldM left) (UnfoldM right) =
    UnfoldM (\ step init -> left step init >>= right step)

instance Monad m => Monad (UnfoldM m) where
  return = pure
  {-# INLINE (>>=) #-}
  (>>=) (UnfoldM left) rightK =
    UnfoldM $ \ step init ->
    let
      newStep output x =
        case rightK x of
          UnfoldM right ->
            right step output
      in left newStep init

instance Monad m => MonadPlus (UnfoldM m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans UnfoldM where
  lift m = UnfoldM (\ step init -> m >>= step init)

instance Monad m => Semigroup (UnfoldM m a) where
  (<>) = (<|>)

instance Monad m => Monoid (UnfoldM m a) where
  mempty = empty
  mappend = (<>)

instance Foldable (UnfoldM Identity) where
  {-# INLINE foldMap #-}
  foldMap inputMonoid = foldl' step mempty where
    step monoid input = mappend monoid (inputMonoid input)
  foldl = foldl'
  {-# INLINE foldl' #-}
  foldl' step init (UnfoldM run) =
    runIdentity (run identityStep init)
    where
      identityStep state input = return (step state input)

instance Eq a => Eq (UnfoldM Identity a) where
  (==) left right = toList left == toList right

instance Show a => Show (UnfoldM Identity a) where
  show = show . toList

{-| Check whether it's empty -}
{-# INLINE null #-}
null :: Monad m => UnfoldM m input -> m Bool
null (UnfoldM run) = run (\ _ _ -> return False) True

{-| Perform a monadic strict left fold -}
{-# INLINE foldlM' #-}
foldlM' :: Monad m => (output -> input -> m output) -> output -> UnfoldM m input -> m output
foldlM' step init (UnfoldM run) =
  run step init

{-| A more efficient implementation of mapM_ -}
{-# INLINE mapM_ #-}
mapM_ :: Monad m => (input -> m ()) -> UnfoldM m input -> m ()
mapM_ step = foldlM' (const step) ()

{-| Same as 'mapM_' with arguments flipped -}
{-# INLINE forM_ #-}
forM_ :: Monad m => UnfoldM m input -> (input -> m ()) -> m ()
forM_ = flip mapM_

{-| Apply a Gonzalez fold -}
{-# INLINE fold #-}
fold :: Fold input output -> UnfoldM Identity input -> output
fold (Fold step init extract) = extract . foldl' step init

{-| Apply a monadic Gonzalez fold -}
{-# INLINE foldM #-}
foldM :: Monad m => FoldM m input output -> UnfoldM m input -> m output
foldM (FoldM step init extract) view =
  do
    initialState <- init
    finalState <- foldlM' step initialState view
    extract finalState

{-| Lift a fold input mapping function into a mapping of unfolds -}
{-# INLINE mapFoldMInput #-}
mapFoldMInput :: Monad m => (forall x. FoldM m b x -> FoldM m a x) -> UnfoldM m a -> UnfoldM m b
mapFoldMInput newFoldM unfoldM = UnfoldM $ \ step init -> foldM (newFoldM (FoldM step (return init) return)) unfoldM

{-| Construct from any foldable -}
{-# INLINE foldable #-}
foldable :: (Monad m, Foldable foldable) => foldable a -> UnfoldM m a
foldable foldable = UnfoldM (\ step init -> A.foldlM step init foldable)

{-| Construct from a specification of how to execute a left-fold -}
{-# INLINE foldlRunner #-}
foldlRunner :: Monad m => (forall x. (x -> a -> x) -> x -> x) -> UnfoldM m a
foldlRunner run = UnfoldM (\ stepM state -> run (\ stateM a -> stateM >>= \state -> stepM state a) (return state))

{-| Construct from a specification of how to execute a right-fold -}
{-# INLINE foldrRunner #-}
foldrRunner :: Monad m => (forall x. (a -> x -> x) -> x -> x) -> UnfoldM m a
foldrRunner run = UnfoldM (\ stepM -> run (\ x k z -> stepM z x >>= k) return)

{-| Filter the values given a predicate -}
{-# INLINE filter #-}
filter :: Monad m => (a -> m Bool) -> UnfoldM m a -> UnfoldM m a
filter test (UnfoldM run) = UnfoldM (\ step -> run (\ state element -> test element >>= bool (return state) (step state element)))

{-| Ints in the specified inclusive range -}
{-# INLINE intsInRange #-}
intsInRange :: Monad m => Int -> Int -> UnfoldM m Int
intsInRange from to =
  UnfoldM $ \ step init ->
  let
    loop !state int =
      if int <= to
        then do
          newState <- step state int
          loop newState (succ int)
        else return state
    in loop init from

{-| TVar contents -}
{-# INLINE tVarValue #-}
tVarValue :: TVar a -> UnfoldM STM a
tVarValue var = UnfoldM $ \ step state -> do
  a <- readTVar var
  step state a

{-| Change the base monad using invariant natural transformations -}
{-# INLINE hoist #-}
hoist :: (forall a. m a -> n a) -> (forall a. n a -> m a) -> UnfoldM m a -> UnfoldM n a
hoist trans1 trans2 (UnfoldM unfold) = UnfoldM $ \ step init -> 
  trans1 (unfold (\ a b -> trans2 (step a b)) init)

{-# INLINABLE byteStringBytes #-}
byteStringBytes :: ByteString -> UnfoldM IO Word8
byteStringBytes (ByteString.PS fp off len) =
  UnfoldM $ \ step init ->
  withForeignPtr fp $ \ ptr ->
  let
    endPtr = plusPtr ptr (off + len)
    iterate !state !ptr = if ptr == endPtr
      then return state
      else do
        x <- peek ptr
        newState <- step state x
        iterate newState (plusPtr ptr 1)
    in iterate init (plusPtr ptr off)

{-# INLINE shortByteStringBytes #-}
shortByteStringBytes :: Monad m => ShortByteString -> UnfoldM m Word8
shortByteStringBytes (ShortByteString.SBS ba#) = primArray (PrimArray ba#)

{-# INLINE primArray #-}
primArray :: (Monad m, Prim prim) => PrimArray prim -> UnfoldM m prim
primArray pa = UnfoldM $ \ f z -> foldlPrimArrayM' f z pa
