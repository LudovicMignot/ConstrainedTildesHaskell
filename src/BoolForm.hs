{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module BoolForm where

import Data.Finite (Finite, getFinite, packFinite)
import Data.List (subsequences)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.TypeNats (KnownNat)
import ToString (ToString (toString))

data BoolForm a
  = Atom a
  | Bot
  | Top
  | And (BoolForm a) (BoolForm a)
  | Or (BoolForm a) (BoolForm a)
  | Not (BoolForm a)
  deriving (Functor, Foldable, Traversable, Eq, Ord)

antiEval :: Eq a => [a] -> BoolForm a -> Bool
antiEval as (Atom a) = a `notElem` as
antiEval _ Bot = False
antiEval _ Top = True
antiEval as (And f1 f2) = antiEval as f1 && antiEval as f2
antiEval as (Or f1 f2) = antiEval as f1 || antiEval as f2
antiEval as (Not f) = not $ antiEval as f

atoms :: Ord a => BoolForm a -> Set a
atoms (Atom a) = Set.singleton a
atoms Bot = Set.empty
atoms Top = Set.empty
atoms (And f1 f2) = atoms f1 `Set.union` atoms f2
atoms (Or f1 f2) = atoms f1 `Set.union` atoms f2
atoms (Not f) = atoms f

antiAllSat :: KnownNat n => BoolForm (Finite n) -> [[Finite n]]
antiAllSat f = filter (`antiEval` f) $ subsequences [0 ..]

smartNeg :: BoolForm a -> BoolForm a
smartNeg Top = Bot
smartNeg Bot = Top
smartNeg (Not f) = f
smartNeg f = Not f

smartOr :: BoolForm a -> BoolForm a -> BoolForm a
smartOr Top _ = Top
smartOr _ Top = Top
smartOr Bot f = f
smartOr f Bot = f
smartOr f g = Or f g

smartAnd :: BoolForm a -> BoolForm a -> BoolForm a
smartAnd Top f = f
smartAnd f Top = f
smartAnd Bot _ = Bot
smartAnd _ Bot = Bot
smartAnd f g = And f g

isSingle :: BoolForm a -> Bool
isSingle Bot = True
isSingle Top = True
isSingle (Atom _) = True
isSingle _ = False

paren :: [Char] -> [Char]
paren s = "(" ++ s ++ ")"

instance Show a => Show (BoolForm a) where
  show Top = "⊤"
  show Bot = "⊥"
  show (Atom a) = show a
  show (Not f@(Not _)) = "¬" ++ show f
  show (Not f)
    | isSingle f = "¬" ++ show f
    | otherwise = "¬" ++ paren (show f)
  show (Or f1 f2@(Or _ _)) = show f1 ++ "∨" ++ paren (show f2)
  show (Or f1 f2) = show f1 ++ "∨" ++ show f2
  show (And f1 f2) = sf1 ++ "∧" ++ sf2
    where
      sf1 = case f1 of
        Or _ _ -> paren $ show f1
        _ -> show f1
      sf2 = case f2 of
        Or _ _ -> paren $ show f2
        And _ _ -> paren $ show f2
        _ -> show f2

instance ToString a => ToString (BoolForm a) where
  toString Top = "⊤"
  toString Bot = "⊥"
  toString (Atom a) = toString a
  toString (Not f@(Not _)) = "¬" ++ toString f
  toString (Not f)
    | isSingle f = "¬" ++ toString f
    | otherwise = "¬" ++ paren (toString f)
  toString (Or f1 f2@(Or _ _)) = toString f1 ++ "∨" ++ paren (toString f2)
  toString (Or f1 f2) = toString f1 ++ "∨" ++ toString f2
  toString (And f1 f2) = sf1 ++ "∧" ++ sf2
    where
      sf1 = case f1 of
        Or _ _ -> paren $ toString f1
        _ -> toString f1
      sf2 = case f2 of
        Or _ _ -> paren $ toString f2
        And _ _ -> paren $ toString f2
        _ -> toString f2

instance {-# OVERLAPPING #-} Show (BoolForm (Finite n)) where
  show Top = "⊤"
  show Bot = "⊥"
  show (Atom a) = show $ getFinite a
  show (Not f@(Not _)) = "¬" ++ show f
  show (Not f)
    | isSingle f = "¬" ++ show f
    | otherwise = "¬" ++ paren (show f)
  show (Or f1 f2@(Or _ _)) = show f1 ++ "∨" ++ paren (show f2)
  show (Or f1 f2) = show f1 ++ "∨" ++ show f2
  show (And f1 f2) = sf1 ++ "∧" ++ sf2
    where
      sf1 = case f1 of
        Or _ _ -> paren $ show f1
        _ -> show f1
      sf2 = case f2 of
        Or _ _ -> paren $ show f2
        And _ _ -> paren $ show f2
        _ -> show f2

subst :: Eq a => BoolForm a -> a -> BoolForm a -> BoolForm a
subst _ _ Bot = Bot
subst _ _ Top = Top
subst f a (Atom b)
  | b == a = f
  | otherwise = Atom b
subst f a (And f1 f2) = And (subst f a f1) (subst f a f2)
subst f a (Or f1 f2) = Or (subst f a f1) (subst f a f2)
subst f a (Not f1) = Not (subst f a f1)

reduce :: BoolForm a -> BoolForm a
reduce Bot = Bot
reduce Top = Top
reduce f@(Atom _) = f
reduce (And f g) =
  case reduce f of
    Bot -> Bot
    Top -> reduce g
    f' -> case reduce g of
      Bot -> Bot
      Top -> f'
      g' -> And f' g'
reduce (Or f g) =
  case reduce f of
    Bot -> reduce g
    Top -> Top
    f' -> case reduce g of
      Bot -> f'
      Top -> Top
      g' -> Or f' g'
reduce (Not f) =
  case reduce f of
    Bot -> Top
    Top -> Bot
    f' -> Not f'

rename :: (a -> b) -> BoolForm a -> BoolForm b
rename = fmap

toFinite :: KnownNat n => BoolForm Integer -> Maybe (BoolForm (Finite n))
toFinite = mapM packFinite

equiv :: BoolForm a -> BoolForm a -> BoolForm a
equiv f f' = And f f' `Or` And (Not f) (Not f')

mirror :: [BoolForm a] -> Maybe (BoolForm a)
mirror [] = Just Top
mirror [_] = Nothing
mirror (f : fs) =
  let f' = last fs
   in fmap (equiv f f' `smartAnd`) $ mirror $ init fs
