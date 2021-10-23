{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators, DeriveFunctor, DataKinds, GADTs
           , StandaloneDeriving,  KindSignatures
           , NoStarIsType #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

module Trie where

import Data.Kind
import Misc

data  U           a = U

data  Id          a = Id a

data  (f  :*  g)  a = X (f a) (g a)

data  (g  :.  f)  a = O (g (f a))



{-

-- Trie isomorphisms

Arr 0          ≅ U

Arr 1          ≅ Id

Arr (m  +  n)  ≅ Arr m  :*  Arr n

Arr (m  *  n)  ≅ Arr n  :.  Arr m


-}

data Arr :: Nat -> Type -> Type

infix 1 ≅

data (≅) :: (Type -> Type) -> (Type -> Type) -> Type where
  Iso :: (forall a . f a -> g a) -> (forall a . g a -> f a) -> f ≅ g
  -- Plus isomorphism proof

type One = Succ Zero

zero  :: Arr Zero ≅ U

one   :: Arr One ≅ Id

(+~)  :: Arr m ≅ f -> Arr n ≅ g -> Arr (m  +  n) ≅ f  :*  g

(*~)  :: Arr m ≅ f -> Arr n ≅ g -> Arr (m  *  n) ≅ g  :.  f

arr   :: Arr m ≅ Arr m


zero = undefined
one  = undefined
(+~) = undefined
(*~) = undefined
arr  = undefined
