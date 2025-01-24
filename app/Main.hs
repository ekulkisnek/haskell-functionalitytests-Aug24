
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

module Main where

import Data.Function
import Data.Functor.Contravariant
import Control.Monad
import Data.List (foldl')
import qualified Data.Map as Map

-- Advanced type-level programming
data Nat = Zero | Succ Nat

type family Add (a :: Nat) (b :: Nat) :: Nat where
  Add Zero b = b
  Add (Succ a) b = Succ (Add a b)

-- GADT example
data Vector a n where
  VNil :: Vector a Zero
  VCons :: a -> Vector a n -> Vector a (Succ n)

-- Type class for custom show
class ShowVector a where
  showVector :: Vector a n -> String

instance Show a => ShowVector a where
  showVector VNil = "[]"
  showVector (VCons x xs) = show x ++ " : " ++ showVector xs

-- Monadic computations
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State h) = State $ \s -> let (a, s') = h s in (f a, s')

instance Applicative (State s) where
  pure x = State $ \s -> (x, s)
  State f <*> State x = State $ \s ->
    let (f', s') = f s
        (x', s'') = x s'
    in (f' x', s'')

instance Monad (State s) where
  return = pure
  State h >>= f = State $ \s ->
    let (a, s') = h s
        State g = f a
    in g s'

-- Example of a complex data structure
data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

-- Tree operations
insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
  | x < y = Node y (insert x left) right
  | otherwise = Node y left (insert x right)

-- Example of fold
treeToList :: Tree a -> [a]
treeToList = go []
  where
    go acc Empty = acc
    go acc (Node x left right) = go (x : go acc right) left

-- Fibonacci using fix point
fib :: Integer -> Integer
fib = fix $ \f n -> case n of
  0 -> 0
  1 -> 1
  n -> f (n-1) + f (n-2)

-- Main demonstration
main :: IO ()
main = do
  putStrLn "Haskell Features Demonstration"
  putStrLn "=============================="
  
  -- Tree demonstration
  let tree = foldl' (flip insert) Empty [5,3,7,1,9,2,6,4,8]
  putStrLn $ "Tree as list: " ++ show (treeToList tree)
  
  -- Fibonacci demonstration
  putStrLn $ "First 10 Fibonacci numbers: " ++ show [fib n | n <- [0..9]]
  
  -- State monad example
  let counterState = do
        modify (+1)
        x <- get
        modify (+2)
        y <- get
        return (x, y)
      modify f = State $ \s -> ((), f s)
      get = State $ \s -> (s, s)
      (result, finalState) = runState counterState 0
  
  putStrLn $ "State monad example: " ++ show result ++ " with final state: " ++ show finalState

  -- Vector example
  let vec = VCons 1 (VCons 2 (VCons 3 VNil))
  putStrLn $ "Vector example: " ++ showVector vec
