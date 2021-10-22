{-# LANGUAGE TypeOperators, DeriveFunctor, DataKinds, GADTs
           , CPP, StandaloneDeriving,  KindSignatures #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

import Misc

data P a = a :# a deriving Functor

data Td a = L a | B (P (Td a)) deriving Functor

scanTd :: Monoid a => Td a -> Td a × a
scanTd (L x) = (L mempty , x)
scanTd (B (u :# v)) = (B (u' :# fmap (totu <>) v') , totu <> totv)
  where
    (u', totu)  = scanTd u
    (v', totv)  = scanTd v
