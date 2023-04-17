{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module BoolForm where

import Data.Finite
import ToString

data BoolForm a
  = Atom a
  | Bot
  | Top
  | And (BoolForm a) (BoolForm a)
  | Or (BoolForm a) (BoolForm a)
  | Not (BoolForm a)
  deriving (Functor, Eq, Ord)

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

equiv :: BoolForm a -> BoolForm a -> BoolForm a
equiv f f' = And f f' `Or` And (Not f) (Not f')

mirror :: [BoolForm a] -> Maybe (BoolForm a)
mirror [] = Just Top
mirror [_] = Nothing
mirror (f : fs) =
  let f' = last fs
   in fmap (equiv f f' `And`) $ mirror $ init fs
