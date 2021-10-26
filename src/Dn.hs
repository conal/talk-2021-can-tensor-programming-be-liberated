{-# LANGUAGE TypeOperators, DeriveFunctor, DataKinds, GADTs
           , CPP, StandaloneDeriving,  KindSignatures #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

import Data.Kind
import Misc

data P a = a :# a deriving Functor

data Tu :: Nat -> Type -> Type where
  L  :: a -> Tu 0 a
  B  :: Tu d (P a) -> Tu (d + 1) a
deriving instance Functor (Tu d)

zipWithTu :: (a -> b -> c) -> (Tu d a -> Tu d b -> Tu d c)
zipWithTu = undefined

unzipTu :: Tu d (a , b) -> Tu d a × Tu d b
unzipTu = undefined

scanP :: Monoid a => P a -> P a × a
scanP (x :# y) = (mempty :# x , y)

scanTu :: Monoid a => Tu d a -> Tu d a × a
scanTu (L x) = (L mempty , x)
scanTu (B ps) = (B (zipWithTu tweak tots' ps'), tot)
  where
    (ps', tots)   = unzipTu (fmap scanP ps)
    (tots', tot)  = scanTu tots
    tweak x       = fmap (x ⊕)
