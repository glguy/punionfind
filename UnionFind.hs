{-# LANGUAGE FlexibleContexts, GADTs #-}
module UnionFind (UnionFind, newUnionFind, union, find) where

import RevertArray (RArray)
import PersistentArray (PersistentArray(..))
import Control.Monad.ST
import Data.STRef
import Ref

data UnionFind a s where
   UF :: PersistentArray (a Int s) Int s
      => STRef s (a Int s) -> a Int s -> UnionFind a s

newUnionFind :: PersistentArray (a Int s) Int s => Int -> ST s (UnionFind a s)
newUnionFind n = do
  pref  <- newRef =<< create n id
  ranks <- create n (const 0)
  return (UF pref ranks)

find :: UnionFind a s -> Int -> ST s Int
find (UF pref _) i = modifyRefM pref (\ arr -> go arr i)
  where
  go parents i = do
    p <- get parents i
    if p == i
      then return (parents, p)
      else do (parents', p') <- go parents p
              parents'' <- set parents i p
              return (parents'', p')

union :: UnionFind a s -> Int -> Int -> ST s (UnionFind a s)
union uf x y = do
  cx <- find uf x
  cy <- find uf y
  if cx == cy then return uf else do
    rx <- rank uf x
    ry <- rank uf y
    case compare rx ry of
      GT -> link uf cy cx
      LT -> link uf cx cy
      _  -> do uf <- incRank uf cx
               link uf cy cx

rank :: UnionFind a s -> Int -> ST s Int
rank (UF _ ranks) x = get ranks x

link :: UnionFind a s -> Int -> Int -> ST s (UnionFind a s)
link (UF pref rref) x y = do
  parents <- getRef pref
  pref'   <- newRef =<< set parents y x
  return (UF pref' rref)

incRank :: UnionFind a s -> Int -> ST s (UnionFind a s)
incRank (UF pref ranks) x = do
  r      <- get ranks x
  ranks' <- set ranks x (r+1)
  return (UF pref ranks')


type MyUnionFind = UnionFind RArray

testuf = do
  uf <- newUnionFind 5 :: ST s (MyUnionFind s)
  a <- find uf 2
  b <- find uf 3
  uf1 <- union uf 2 3
  c <- find uf1 2
  d <- find uf1 3
  e <- find uf 2
  f <- find uf 3
  return (a,b,c,d,e,f)
