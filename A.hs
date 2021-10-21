{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wall #-}

infixr 6 ×
type a × b = (a , b)

data Td a = L a | B (Td a) (Td a)
  deriving Functor

scanTd :: Monoid a => Td a -> Td a × a
scanTd (L x) = (L mempty , x)
scanTd (B u v) = (B u' (fmap (ou <>) v') , ou <> ov)
 where
   (u', ou) = scanTd u
   (v', ov) = scanTd v
