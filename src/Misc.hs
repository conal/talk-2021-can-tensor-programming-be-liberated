{-# LANGUAGE TypeOperators, DataKinds #-}
{-# OPTIONS_GHC -Wall #-}

module Misc where

infixr 6 ×
type a × b = (a , b)

data Nat = Zero | Succ Nat

infixl 6 ⊕
(⊕) :: Monoid m => m -> m -> m
(⊕) = (<>)
