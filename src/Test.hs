{-# LANGUAGE DataKinds #-}

module Test where

import BoolForm
import Data.Finite
import Data.Maybe
import Data.Set as S
import Data.Vector.Sized as V
import Exp
import ExpFromString
import NFA ( NFA, faToDot, faToPng )

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

plus :: a -> Exp a
plus a = Symbol a `Concat` Star (Symbol a)

phi' :: BoolForm (Finite 4)
phi' = fromJust $ mirror [Atom 0, Atom 1, Atom 2, Atom 3]

expr :: Exp Char
expr = ConsTilde phi' $ fromTuple (plus 'a', plus 'b', plus 'c', plus 'd')

auto :: NFA (Exp Char) Char
auto = antimirov expr

viz :: String
viz = faToDot auto

vizPng :: IO FilePath
vizPng = faToPng "test" auto

expr2 :: Exp Char
expr2 = fromJust $ expFromString "|(0&3 Or ¬0 And ~3) * (1 & 2 ∨ Not 1 ∧ ! 2)|(a.a*, b.b*, a.a*, b.b*)"

vizPng2 :: IO FilePath
vizPng2 = faToPng "test2" $ antimirov expr2