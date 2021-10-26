{-# LANGUAGE TypeOperators, DeriveFunctor, DataKinds, GADTs
           , TypeFamilies , StandaloneDeriving, KindSignatures
           , NoStarIsType #-}
{-# OPTIONS_GHC -Wall #-}

module Misc (module Misc, module GHC.TypeNats) where

import GHC.TypeNats

type Unit = ()

infixr 6 ×
type a × b = (a , b)

infixl 6 ⊕
(⊕) :: Monoid m => m -> m -> m
(⊕) = (<>)
