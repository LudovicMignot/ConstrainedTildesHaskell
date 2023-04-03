{-# LANGUAGE DataKinds #-}
module Test where

import BoolForm
import Exp
import Data.Finite
import Data.Vector.Sized
import Data.Set as S


phi :: BoolForm (Finite 3)
phi = And (Or (Atom (finite 2)) (Atom (finite 0))) (Not (Atom (finite 1)))

e :: Exp String
e = ConsTilde phi $ fromTuple (Symbol "a", Symbol "b", Symbol "c")

res :: Set (Exp String)
res = allDeriveBySymbs (S.fromList ["a", "b", "c"]) e