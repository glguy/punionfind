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

import Control.Monad.ST
import Data.STRef

import PersistentArray
import RevertArray

data UnionFind a s = UF (STRef s (a s)) (a s)

newUnionFind :: PersistentArray a => Int -> ST s (UnionFind a s)
newUnionFind n = do
  p <- newSTRef =<< newArr n id
  r <- newArr n (const 0)
  return (UF p r)

find :: PersistentArray a => UnionFind a s -> Int -> ST s Int
find (UF pref _) i = do
  ps0 <- readSTRef pref

  let go i f = do
        p <- getArr i ps0
        if p == i
            then f i ps0
            else go p $ \ ci ps -> do
                   ps <- setArr i ci ps
                   f p ps
            
  go i $ \ ci ps -> do
    writeSTRef pref ps
    return ci

union :: PersistentArray a 
      => UnionFind a s -> Int -> Int -> ST s (UnionFind a s)
union uf@(UF p r) x y = do
  cx <- find uf x
  cy <- find uf y
  if cx == cy then return uf else do
    rx <- getArr cx r
    ry <- getArr cy r
    case compare rx ry of
      GT -> link cy cx
      LT -> link cx cy
      EQ -> setRank cx (rx + 1) =<< link cy cx
  where
    link x y = do
      p' <- newSTRef =<< setArr y x =<< readSTRef p
      return (UF p' r)

setRank :: PersistentArray a
        => Int -> Int -> UnionFind a s -> ST s (UnionFind a s)
setRank x n (UF p r) = do
  r' <- setArr x n r
  return (UF p r')

modifySTRefM :: STRef s a -> (a -> ST s (a,b)) -> ST s b
modifySTRefM ref f = do
     (x,res) <- f =<< readSTRef ref
     writeSTRef ref x
     return res
