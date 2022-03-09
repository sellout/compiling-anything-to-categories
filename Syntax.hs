{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | The simplest example using the "Categorifier" plugin.

import qualified Categorifier.Categorify as Categorify
import qualified Categorifier.ConCat.Examples.Syntactic as Syntactic
import qualified Control.Lens as Lens

Categorify.function 'Lens.view [t|Syntactic.Syn|] []

main :: IO ()
main = putStrLn . Syntactic.render $ wrap_view @Int @((->) Int)
