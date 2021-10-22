{-# LANGUAGE TypeOperators, DeriveFunctor, CPP #-}
{-# OPTIONS_GHC -Wall #-}

#define SPC

infixr 6 ×
type a × b = (a , b)

data Td a = L a | B (Td a) (Td a)
  deriving Functor

type Z = [] SPC ()

scanTd :: Monoid a => Td a -> Td a × a
scanTd (L x) = (L mempty , x)
scanTd (B u v) = (B u' (fmap (totu SPC <>) v') , totu <> totv)
 where
   (u', totu) = scanTd u
   (v', totv) = scanTd v
