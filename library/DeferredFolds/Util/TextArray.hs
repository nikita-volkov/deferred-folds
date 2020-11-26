module DeferredFolds.Util.TextArray
where

import DeferredFolds.Prelude
import qualified Data.Text.Internal as TextInternal
import qualified Data.Text.Internal.Encoding.Utf16 as TextUtf16
import qualified Data.Text.Internal.Unsafe.Char as TextChar
import qualified Data.Text.Array as TextArray


{-|
Same as 'Data.Text.Unsafe.iter',
but operates on the array directly.
-}
{-# INLINE iter #-}
iter :: (Char -> Int -> a) -> Int -> TextArray.Array -> a
iter cont offset arr =
  let
    b1 =
      TextArray.unsafeIndex arr offset
    in if b1 >= 0xd800 && b1 <= 0xdbff
      then let
        b2 =
          TextArray.unsafeIndex arr (succ offset)
        char =
          TextUtf16.chr2 b1 b2
        in cont char (offset + 2)
      else
        cont (TextChar.unsafeChr b1) (offset + 1)
