{-# LANGUAGE TypeOperators, DeriveFunctor, CPP #-}
{-# OPTIONS_GHC -Wall #-}

#define SPC

infixr 6 ×
type a × b = (a , b)


data P a = a :# a deriving Functor

data Tu a = L a | B (Tu (P a)) deriving Functor

zipWithTu :: (a -> b -> c) -> (Tu a -> Tu b -> Tu c)
zipWithTu = undefined

unzipTu :: Tu (a , b) -> Tu a × Tu b
unzipTu = undefined

-- Oh! I can't define zipWithTu sensibly without matching shapes.

scanP :: Monoid a => P a -> P a × a
scanP (x :# y) = (mempty :# x , y)

scanTu :: Monoid a => Tu a -> Tu a × a
scanTu (L x) = (L mempty , x)
scanTu (B ps) = (B (zipWithTu tweak tots' ps'), tot)
  where
    (ps', tots)   = unzipTu (fmap scanP ps)
    (tots', tot)  = scanTu tots
    tweak x       = fmap (x SPC <>)
