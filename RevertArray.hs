{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

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
import Ref

-- | RevertArray is a persistent array that allows a previous revision
--   to be used at the cost of invalidating all subsequent revisions.
--   This is intended to be used in backtracking situations where an
--   efficient revert history is needed. The programmer is responsible
--   for ensuring that a RevertArray value is not used once invalidated.
newtype RevertArray e s = RA (STRef s (ArrayHistory e s))

data ArrayHistory e s
  = Arr  !(MUArr e s)                  -- ^ Latest version of the array
  | Diff !(RevertArray e s) !Int !e    -- ^ The array has been modified
  | Invalid                            -- ^ The array has been reverted
  
instance UA e => PersistentArray (RevertArray e s) e (ST s) where
  newArr = new
  getArr = get
  setArr = set

new :: UA e => Int -> (Int -> e) -> ST s (RevertArray e s)
new n f = do
  arr <- newMU n
  mapM_ (\ i -> writeMU arr i (f i)) [0..n-1]
  RA `fmap` newRef (Arr arr)

get :: UA e => RevertArray e s -> Int -> ST s e
get (RA ref) i = do
  h <- getRef ref
  case h of
    Arr arr -> readMU arr i
    Invalid -> error "get: invalid array"
    Diff {} -> do
      arr <- revert ref
      setRef ref (Arr arr)
      readMU arr i

set :: UA e => RevertArray e s -> Int -> e -> ST s (RevertArray e s)
set (RA ref) i e = do
  arr <- revert ref
  old <- readMU arr i
  writeMU arr i e
  r <- RA `fmap` newRef (Arr arr)
  setRef ref (Diff r i old)
  return r

revert :: UA e => STRef s (ArrayHistory e s) -> ST s (MUArr e s)
revert ref = do
  h <- getRef ref
  case h of
    Arr arr -> return arr
    Invalid -> error "revert: invalid array"
    Diff (RA r) i e -> do
      arr <- revert r
      writeMU arr i e
      setRef r Invalid
      return arr
