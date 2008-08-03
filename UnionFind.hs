{- |
Module      :  UnionFind
Description :  Efficient backtracking union-find data structure
Copyright   :  (c) 2008 Eric Mertens
License     :  BSD3

Maintainer  :  emertens@gmail.com
Stability   :  provisional
Portability :  non-portable

-}

module UnionFind (UnionFind, newUnionFind, union, find) where

import Control.Monad
import Control.Monad.ST
import Data.STRef

import PersistentArray
import RevertArray

data UnionFind a s = UF (STRef s (a s)) (a s)

newUnionFind :: PersistentArray a => Int -> ST s (UnionFind a s)
newUnionFind n = liftM2 UF (newSTRef =<< newArr n id) (newArr n (const 0))

find :: PersistentArray a => UnionFind a s -> Int -> ST s Int
find (UF pref _) i = modifySTRefM pref (go i)
    where
      go i parents = do
        p <- getArr parents i
        if p == i then return (parents, p) else do
           (parents, p) <- go p parents
           parents      <- setArr parents i p
           return (parents, p)

union :: PersistentArray a 
      => UnionFind a s -> Int -> Int -> ST s (UnionFind a s)
union uf x y = do
  cx <- find uf x
  cy <- find uf y
  if cx == cy then return uf else do
    rx <- rank uf cx
    ry <- rank uf cy
    case compare rx ry of
      GT -> link uf cy cx
      LT -> link uf cx cy
      EQ -> do uf <- setRank uf cx (rx + 1)
               link uf cy cx

  where
    rank (UF _ ranks) x = getArr ranks x

    link (UF pref rref) x y = do
      parents <- readSTRef pref
      pref'   <- newSTRef =<< setArr parents y x
      return (UF pref' rref)

    setRank (UF pref ranks) x r = UF pref `liftM` setArr ranks x r

modifySTRefM :: STRef s a -> (a -> ST s (a,b)) -> ST s b
modifySTRefM ref f = do
     (x,res) <- f =<< readSTRef ref
     writeSTRef ref x
     return res
