{-# LANGUAGE UnboxedTuples #-}
module DoubleHashOp where

(##) :: a -> b -> Int
a ## b = 0

(#*) :: a -> b -> Int
a #* b = 1