{-# LANGUAGE TypeOperators, DeriveFunctor, DataKinds, GADTs
           , StandaloneDeriving,  KindSignatures #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

import Misc

data Td :: Nat -> * -> * where
  L  :: a -> Td Zero a
  B  :: Td d a -> Td d a -> Td (Succ d) a

deriving instance Functor (Td d)

scanTd :: Monoid a => Td d a -> Td d a × a
scanTd (L x)    = (L mempty , x)
scanTd (B u v)  = (B u' (fmap (utot ⊕) v') , utot <> vtot)
  where
    (u'  , utot  ) = scanTd u
    (v'  , vtot  ) = scanTd v
