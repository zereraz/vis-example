module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CanvasElement, Context2D, getCanvasElementById, getContext2D)
import Vis.Core (createAnim)
import Vis.Types (AnimationOperation(..), Effs)
import Vis.Utils (subTree, tree)

init :: Eff Effs Unit
init = do
  maybeCanvas <- getCanvasElementById "canvas"
  case maybeCanvas of
       Just c -> getContext2D c >>= initCanvas c
       Nothing -> log "no canvas found"

-- | Create stateStream - stream of events for any new state
-- | Setup update loop and animation operations loop
initCanvas :: CanvasElement -> Context2D -> Eff (Effs) Unit
initCanvas c ctx = do
-- | graphic variables containing their own animation cycle and update cycle
  g <- createAnim ctx (tree [Translate 0.0 1.0, Translate 1.0 0.0]) 60
  g1 <- createAnim ctx (subTree [Translate 1.0 0.0]) 60
  pure unit


main :: Eff (Effs) Unit
main = do
  init
