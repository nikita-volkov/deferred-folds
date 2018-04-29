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

-- foldl
-------------------------
import Control.Foldl as Exports (Fold(..))
