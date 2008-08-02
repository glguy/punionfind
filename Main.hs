module Main where

import Control.Monad
import BacktrackingUnionFind

main = print $ runM 5 test

test :: M s Int
test = do
  unify 2 3
  x <- option [0..4]
  unify 0 x
  guard       =<< testEquiv 0 3
  guard . not =<< testEquiv 0 1
  return x

option :: MonadPlus m => [a] -> m a
option = msum . map return
