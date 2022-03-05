{-# LANGUAGE TemplateHaskell #-}

-- | The simplest example using the "Categorifier" plugin.

import qualified Categorifier.Categorify as Categorify
import Control.Category (Category (..))
import Control.Arrow (Arrow (..))
import Prelude hiding ((.), id)

-- | This is our category. It simply wraps @->@ in a @newtype@, so the semantics
--   are obvious.
newtype Hask a b = Hask {runHask :: a -> b}

-- | These instances tell us what the categorical (or Haskell) operations mean
--   in the target category. Again, in this case they're all trivial.
instance Category Hask where
  id = Hask id
  Hask g . Hask f = Hask $ g . f

-- | `Arrow` is a pretty intense class. Most of the time you won't be able to
--   get away with just defining this and letting everything work. There are
--   other type class hierarchies that you can use instead of base that give
--   more fine-grained definitions. That is usually what you'll want.
instance Arrow Hask where
  arr = Hask
  Hask f *** Hask g = Hask $ f *** g

-- | This then tells us to convert the function
-- > negate :: `Num` a => a -> a
--   to
-- > wrap_negate :: `Num` a => `Hask` a a
Categorify.function 'negate [t|Hask|] []

-- | Finally, we use `wrap_negate`. Which, in this trivial case, just means
--   unwrapping it and applying the underlying function.
main :: IO ()
main = print $ runHask wrap_negate (5 :: Int)
