{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import Data.Function
import Data.Functor.Contravariant




main :: IO ()
main = putStrLn "Hello, world!"


val = fix (uncurry (fix (uncurry . curry . flip)))

--  val :: ((a ,b), F a ) uses input a to determine b
--  val :: ((a ,b), F b ) uses output to determine input
--  val :: ((a ,b), F (a,b) ) uses input a and output feedback to determine output

--  val :: (F (a ,b), F (a,b) ) uses injectivity of F to determine fixed point of F 

type D :: (p) -> *
type family D a where
  forall c b a. D (a b c) = (a b b, a c c) 
  D a = (a, a)
  
type family F v where
  F Int = Int
  F (a,b) = (Int,Int)




  
-- v val
-- scoped typevars to refer to recursive types
-- quantified constraints to specify overlapping instances


-- (a,b) 

class V a where
  v :: a -> Int
  
instance V ((a,b),b) where
  v = undefined

instance V (((a,b))) where
  v = undefined



