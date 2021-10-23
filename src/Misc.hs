{-# LANGUAGE TypeOperators, DeriveFunctor, DataKinds, GADTs
           , TypeFamilies , StandaloneDeriving, KindSignatures
           , NoStarIsType #-}
{-# OPTIONS_GHC -Wall #-}

module Misc where


type Unit = ()

infixr 6 ×
type a × b = (a , b)

data Nat = Zero | Succ Nat

infixl 6 +
infixl 7 *

type family (a :: Nat) + (b :: Nat) :: Nat where
type family (a :: Nat) * (b :: Nat) :: Nat where

infixl 6 ⊕
(⊕) :: Monoid m => m -> m -> m
(⊕) = (<>)
