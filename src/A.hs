{-# LANGUAGE TypeOperators, DeriveFunctor, DataKinds, GADTs
           , StandaloneDeriving,  KindSignatures #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

import Data.Kind
import Misc

data Td :: Type -> Type where
  L  :: a -> Td a
  B  :: Td a -> Td a -> Td a

deriving instance Functor Td

scanTd :: Monoid a => Td a -> Td a × a
scanTd (L x) = (L mempty , x)
scanTd (B u v) = (B u' (fmap (utot ⊕) v') , utot <> vtot)
  where
    (u'  , utot  ) = scanTd u
    (v'  , vtot  ) = scanTd v
