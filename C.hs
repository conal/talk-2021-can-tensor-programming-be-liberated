{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- TEMP
{-# LANGUAGE DeriveFunctor #-}

data P a = a :# a deriving Functor

data Tu a = L a | B (Tu (P a)) deriving Functor

zipWithT :: (a -> b -> c) -> (Tu a -> Tu b -> Tu c)
zipWithT = undefined

-- zipWithT f (L x) (L y) = L (f x y)
-- zipWithT f (B us) (B vs) = B (fmap (\ (x :# y) -> f x y)

unzipT :: Tu (a , b) -> (Tu a , Tu b)
unzipT = undefined

-- Oh! I can't define zipWithT sensibly without matching shapes.

scanP :: Monoid a => P a -> (P a , a)
scanP (x :# y) = (mempty :# x , y)

scanT :: Monoid a => Tu a -> (Tu a , a)
scanT (L x) = (L mempty , x)
scanT (B ts) = (B (zipWithT tweak tots' ts'), tot)
  where
    (ts', tots)   = unzipT (fmap scanP ts)
    (tots', tot)  = scanT tots
    tweak t       = fmap (t <>)
