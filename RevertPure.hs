{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module RevertArray where

import Control.Monad.ST
import Data.IORef
import Data.Array.Vector
import PersistentArrayPure (PersistentArrayPure)
import qualified PersistentArrayPure
import System.IO.Unsafe

newtype RArray e s = RArray (IORef (ArrayHistory e))

instance UA e => PersistentArrayPure (RArray e) e where
  create        = create
  get           = get
  set           = set


data ArrayHistory e
  = Arr  !(UArr e)
  | Diff !(RArray e) !Int !e
  | Invalid

{-# NOINLINE create #-}
create n f = unsafePerformIO $ do
  arr <- runST (do arr <- newMU n
                   mapM_ (\ i -> writeMU arr i (f i)) [0..n-1]
                   unsafeFreezeMU arr)
  ref <- newIORef (Arr arr)
  return (RArray ref)


{-# NOINLINE get #-}
get r@(RArray ref) i = unsafePerformIO $ do
  h <- readIORef ref
  case h of
    Arr arr -> readMU arr i
    Diff {} -> do
      arr <- flatten ref
      writeSTRef ref (Arr arr)
      readMU arr i
    _ -> error "get: invalid array"

flatten ref = do
  h <- readSTRef ref
  case h of
    Arr arr -> return arr
    Diff (RArray r) i e -> do
      arr <- flatten r
      writeMU arr i e
      writeSTRef r Invalid
      return arr
    _ -> error "flatten: invalid array"

set (RArray ref) i e = do
  arr <- flatten ref
  old <- readMU arr i
  writeMU arr i e
  r <- RArray `fmap` newSTRef (Arr arr)
  writeSTRef ref (Diff r i old)
  return r

test = do
  arr1 <- create 5 (const 0) :: ST s (RArray Int s)
  arr2 <- set arr1 2 16
  arr3 <- set arr2 2 32
  arr4 <- set arr3 2 48
  a <- get arr4 2
  b <- get arr2 2
  c <- get arr1 2
  return (a,b,c)

main = print $ runST test
