{-# LANGUAGE TypeOperators, DeriveFunctor, DataKinds, GADTs
           , CPP, StandaloneDeriving,  KindSignatures #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

import Data.Kind
import Misc

data P a = a :# a deriving Functor

data Tu :: Nat -> Type -> Type where
  L  :: a -> Tu Zero a
  B  :: Tu n (P a) -> Tu (Succ n) a
deriving instance Functor (Tu n)

zipWithTu :: (a -> b -> c) -> (Tu n a -> Tu n b -> Tu n c)
zipWithTu = undefined

unzipTu :: Tu n (a , b) -> Tu n a × Tu n b
unzipTu = undefined

scanP :: Monoid a => P a -> P a × a
scanP (x :# y) = (mempty :# x , y)

scanTu :: Monoid a => Tu n a -> Tu n a × a
scanTu (L x) = (L mempty , x)
scanTu (B ps) = (B (zipWithTu tweak tots' ps'), tot)
  where
    (ps', tots)   = unzipTu (fmap scanP ps)
    (tots', tot)  = scanTu tots
    tweak x       = fmap (x ⊕)
