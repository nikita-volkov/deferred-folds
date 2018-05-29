module DeferredFolds.Prelude
(
  module Exports,
)
where


-- base
-------------------------
import Prelude as Exports hiding ((<>))
import Foreign as Exports hiding (void)
import Data.Monoid as Exports hiding ((<>), First(..), Last(..))
import Data.Semigroup as Exports
import Data.Foldable as Exports
import Data.Functor.Identity as Exports
import Data.Traversable as Exports
import Control.Applicative as Exports
import Control.Monad as Exports

-- containers
-------------------------
import Data.IntMap.Strict as Exports (IntMap)
import Data.Map.Strict as Exports (Map)
import Data.IntSet as Exports (IntSet)
import Data.Set as Exports (Set)
import Data.Sequence as Exports (Seq)

-- foldl
-------------------------
import Control.Foldl as Exports (Fold(..), FoldM(..))
