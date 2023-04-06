{-# LANGUAGE DataKinds #-}

module Test where

import BoolForm
import Data.Finite
import Data.Set as S
import Data.Vector.Sized as V
import Exp

phi :: BoolForm (Finite 3)
phi = And (Or (Atom (finite 2)) (Atom (finite 0))) (Not (Atom (finite 1)))

e :: Exp Char
e = ConsTilde phi $ fromTuple (Symbol 'a', Symbol 'b', Symbol 'c')

res :: Set (Exp Char)
res = allDeriveBySymbs (S.fromList "abc") e

e' :: Exp a
e' = ConsTilde Bot V.empty

e'' :: Exp a
e'' = Concat Epsilon e'

e3 :: Exp a
e3 = Concat Epsilon $ ConsTilde Top V.empty