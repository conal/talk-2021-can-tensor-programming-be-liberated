{-# LANGUAGE TypeOperators, DeriveFunctor, DataKinds, GADTs
           , CPP, StandaloneDeriving,  KindSignatures #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

import Data.Kind
import Misc

data P a = a :# a deriving Functor

data Td :: Nat -> Type -> Type where
  L  :: a -> Td 0 a
  B  :: P (Td d a) -> Td (d + 1) a

deriving instance Functor (Td d)

scanTd :: Monoid a => Td d a -> Td d a × a
scanTd (L x) = (L mempty , x)
scanTd (B (u :# v)) = (B (u' :# fmap (utot <>) v') , utot <> vtot)
  where
    (u', utot)  = scanTd u
    (v', vtot)  = scanTd v
