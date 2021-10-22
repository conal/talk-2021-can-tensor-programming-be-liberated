{-# LANGUAGE TypeOperators, DeriveFunctor, DataKinds, GADTs
           , StandaloneDeriving,  KindSignatures #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

module Trie where

-- import Misc

data  U            a = U

data  Id           a = Id a

data  (f  :*:  g)  a = X (f a) (g a)

data  (g  :.   f)  a = O (g (f a))



{-

-- Trie isomorphisms

Arr 0          ≅ U

Arr 1          ≅ Id

Arr (m  +  n)  ≅ Arr m :*: Arr n

Arr (m  *  n)  ≅ Arr n :. Arr m


-}

infix 1 :~:

data (:~:) :: (* -> *) -> (* -> *) -> * where
  
