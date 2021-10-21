{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- TEMP
{-# LANGUAGE DeriveFunctor #-}

infixr 6 ×
type a × b = (a , b)

data P a = a :# a deriving Functor

data Td a = L a | B (P (Td a)) deriving Functor

scanTd :: Monoid a => Td a -> Td a × a
scanTd (L x) = (L mempty , x)
scanTd (B (u :# v)) = (B (u' :# fmap (ou <>) v') , ou <> ov)
  where
    (u', ou)  = scanTd u
    (v', ov)  = scanTd v
