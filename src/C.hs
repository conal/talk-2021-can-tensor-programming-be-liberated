{-# LANGUAGE TypeOperators, DeriveFunctor, DataKinds, GADTs
           , CPP, StandaloneDeriving,  KindSignatures #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

import Misc

data P a = a :# a deriving Functor

data Td a = L a | B (P (Td a)) deriving Functor

zipWithP :: (a -> b -> c) -> (P a -> P b -> P c)
zipWithP = undefined

unzipP :: P (a × b) -> P a × P b
unzipP = undefined

-- Oh! I can't define zipWithP without matching shapes (depth indices).

scanP :: Monoid a => P a -> P a × a
scanP (x :# y) = (mempty :# x , y)

scanTd :: Monoid a => Td a -> Td a × a
scanTd (L x) = (L mempty , x)
scanTd (B ts) = (B (zipWithP tweak tots' ts'), tot)
  where
    (ts', tots)   = unzipP (fmap scanTd ts)
    (tots', tot)  = scanP tots
    tweak x       = fmap (x ⊕)
