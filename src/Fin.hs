{-# LANGUAGE TypeOperators, DeriveFunctor, DataKinds, GADTs
           , StandaloneDeriving,  KindSignatures #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

module Fin where

import Misc

-- "Finite sets"
data Fin :: Nat -> * where
  ZeroF  :: Fin (Succ n)
  SuccF  :: Fin n -> Fin (Succ n)

{-

-- Index isomorphisms

Fin 0          ≅ Void

Fin 1          ≅ Unit

Fin (m  +  n)  ≅ Fin m  ⊎   Fin n

Fin (m  *  n)  ≅ Fin m  ×   Fin n

-}

-- Fin (n ^ m)  ≅ Fin m  ->  Fin n
