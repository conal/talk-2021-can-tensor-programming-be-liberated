{-# LANGUAGE TypeOperators, DeriveFunctor, DataKinds, GADTs
           , CPP, StandaloneDeriving,  KindSignatures #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

import Data.Kind
import Misc

data P a = a :# a deriving Functor

data Td :: Nat -> Type -> Type where
  L  :: a -> Td Zero a
  B  :: P (Td d a) -> Td (Succ d) a
deriving instance Functor (Td d)

zipWithP :: (a -> b -> c) -> (P a -> P b -> P c)
zipWithP = undefined

unzipP :: P (a × b) -> P a × P b
unzipP = undefined

scanP :: Monoid a => P a -> P a × a
scanP (x :# y) = (mempty :# x , y)

scanTd :: Monoid a => Td d a -> Td d a × a
scanTd (L x) = (L mempty , x)
scanTd (B ts) = (B (zipWithP tweak tots' ts'), tot)
  where
    (ts', tots)   = unzipP (fmap scanTd ts)
    (tots', tot)  = scanP tots
    tweak x       = fmap (x ⊕)
