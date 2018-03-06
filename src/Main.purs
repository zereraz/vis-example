module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Vis.Core as C
import Vis.Types (Effs)

main :: Eff (Effs) Unit
main = do
  _ <- C.init
  log "Hello sailor!"
