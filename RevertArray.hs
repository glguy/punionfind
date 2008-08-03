{- |
Module      :  RevertArray
Description :  Efficient backtracking array data structure
Copyright   :  (c) 2008 Eric Mertens
License     :  BSD3

Maintainer  :  emertens@gmail.com
Stability   :  provisional
Portability :  non-portable

-}

module RevertArray (RevertArray) where

import Control.Monad.ST
import Data.Array.Vector
import Data.STRef

import PersistentArray

-- | RevertArray is a persistent array that allows a previous revision
--   to be used at the cost of invalidating all subsequent revisions.
--   This is intended to be used in backtracking situations where an
--   efficient revert history is needed. The programmer is responsible
--   for ensuring that a RevertArray value is not used once invalidated.
newtype RevertArray s = RA (STRef s (ArrayHistory s))

data ArrayHistory s
  = Arr  !(MUArr Int s)                -- ^ Latest version of the array
  | Diff !(RevertArray s) !Int !Int    -- ^ The array has been modified
  | Invalid                            -- ^ The array has been reverted

instance PersistentArray RevertArray where
  newArr = new
  getArr = get
  setArr = set

new :: Int -> (Int -> Int) -> ST s (RevertArray s)
new n f = do
  arr <- newMU n
  mapM_ (\ i -> writeMU arr i (f i)) [0..n-1]
  RA `fmap` newSTRef (Arr arr)

get :: RevertArray s -> Int -> ST s Int
get (RA ref) i = do
  h <- readSTRef ref
  case h of
    Arr arr -> readMU arr i
    Invalid -> error "get: invalid array"
    Diff {} -> do
      arr <- revert ref
      writeSTRef ref (Arr arr)
      readMU arr i

set :: RevertArray s -> Int -> Int -> ST s (RevertArray s)
set (RA ref) i e = do
  arr <- revert ref
  old <- readMU arr i
  writeMU arr i e
  r <- RA `fmap` newSTRef (Arr arr)
  writeSTRef ref (Diff r i old)
  return r

revert :: STRef s (ArrayHistory s) -> ST s (MUArr Int s)
revert ref = do
  h <- readSTRef ref
  case h of
    Arr arr -> return arr
    Invalid -> error "revert: invalid array"
    Diff (RA r) i e -> do
      arr <- revert r
      writeMU arr i e
      writeSTRef r Invalid
      return arr
