{-# LANGUAGE TypeOperators, DeriveFunctor, DataKinds, GADTs
           , CPP, StandaloneDeriving,  KindSignatures #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

import Misc

data P a = a :# a deriving Functor

data Td :: Nat -> * -> * where
  L  :: a -> Td Zero a
  B  :: P (Td d a) -> Td (Succ d) a

deriving instance Functor (Td d)

scanTd :: Monoid a => Td d a -> Td d a × a
scanTd (L x) = (L mempty , x)
scanTd (B (u :# v)) = (B (u' :# fmap (totu <>) v') , totu <> totv)
  where
    (u', totu)  = scanTd u
    (v', totv)  = scanTd v
