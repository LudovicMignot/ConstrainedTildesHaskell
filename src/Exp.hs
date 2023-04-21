{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Exp where

import BoolForm hiding (isSingle, paren)
import Data.Data
import Data.Finite
import Data.Foldable as F
import Data.List (intercalate)
import Data.Maybe
import Data.Set as S
import Data.Singletons.Decide
  ( Decision (Disproved, Proved),
    (%~),
  )
import Data.Type.Equality
import Data.Vector.Sized as V hiding ((++))
import FAClass
import GHC.TypeLits.Singletons
import GHC.TypeNats
import NFA
import ToString
import Unsafe.Coerce

-- from https://stackoverflow.com/questions/46634706/haskell-singletons-typelits-package

data IsZero (n :: Nat) where
  Zero :: (0 ~ n) => IsZero n
  NonZero :: (m ~ (n + 1)) => IsZero m

isZero :: forall n. Sing n -> IsZero n
isZero n = case n %~ (SNat @0) of
  Proved Refl -> Zero
  Disproved _ -> unsafeCoerce NonZero

data Exp a where
  Symbol :: a -> Exp a
  Epsilon :: Exp a
  Empty :: Exp a
  Sum :: (Exp a) -> (Exp a) -> Exp a
  Concat :: (Exp a) -> (Exp a) -> Exp a
  Star :: (Exp a) -> Exp a
  ConsTilde :: (BoolForm (Finite n)) -> (Vector n (Exp a)) -> Exp a

consTilde :: BoolForm Integer -> [Exp a] -> Maybe (Exp a)
consTilde f (es :: [Exp a]) = V.withSizedList es aux
  where
    aux :: forall n. KnownNat n => Vector n (Exp a) -> Maybe (Exp a)
    aux v = knownLength v $ flip ConsTilde v <$> toFinite f

alphabet :: Ord a => Exp a -> Set a
alphabet (Symbol a) = S.singleton a
alphabet Epsilon = S.empty
alphabet Empty = S.empty
alphabet (Sum e1 e2) = Exp.alphabet e1 `S.union` Exp.alphabet e2
alphabet (Concat e1 e2) = Exp.alphabet e1 `S.union` Exp.alphabet e2
alphabet (Star e) = Exp.alphabet e
alphabet (ConsTilde _ es) = V.foldl' (\acc e -> Exp.alphabet e `S.union` acc) S.empty es

conc :: Exp a -> Exp a -> Exp a
conc Epsilon e = e
conc e Epsilon = e
conc _ Empty = Empty
conc Empty _ = Empty
conc e f = Concat e f

smartCons :: (BoolForm (Finite n)) -> (Vector n (Exp a)) -> Exp a
smartCons Bot _ = Empty
smartCons f es = ConsTilde f es

instance Eq a => Eq (Exp a) where
  Symbol a == Symbol b = a == b
  Epsilon == Epsilon = True
  Empty == Empty = True
  Sum f1 f2 == Sum f1' f2' = f1 == f1' && f2 == f2'
  Concat f1 f2 == Concat f1' f2' = f1 == f1' && f2 == f2'
  Star f == Star f' = f == f'
  ConsTilde (phi :: BoolForm (Finite n)) es == ConsTilde (phi' :: BoolForm (Finite n')) es' =
    knownLength es $
      knownLength es' $
        case sameNat (Proxy @n) (Proxy @n') of
          Just p -> castWith (apply Refl (apply (Refl :: Finite :~: Finite) p)) phi == phi' && V.toList es == V.toList es'
          Nothing -> False
  _ == _ = False

instance Ord a => Ord (Exp a) where
  Symbol a <= Symbol b = a <= b
  _ <= Symbol _ = False
  Symbol _ <= Epsilon = True
  Epsilon <= Epsilon = True
  _ <= Epsilon = False
  Symbol _ <= Empty = True
  Epsilon <= Empty = True
  Empty <= Empty = True
  _ <= Empty = False
  Symbol _ <= Sum _ _ = True
  Epsilon <= Sum _ _ = True
  Empty <= Sum _ _ = True
  Sum f1 f2 <= Sum f1' f2' = (f1, f2) <= (f1', f2')
  _ <= Sum _ _ = False
  Symbol _ <= Concat _ _ = True
  Epsilon <= Concat _ _ = True
  Empty <= Concat _ _ = True
  Sum _ _ <= Concat _ _ = True
  Concat f1 f2 <= Concat f1' f2' = (f1, f2) <= (f1', f2')
  _ <= Concat _ _ = False
  Symbol _ <= Star _ = True
  Epsilon <= Star _ = True
  Empty <= Star _ = True
  Sum _ _ <= Star _ = True
  Concat _ _ <= Star _ = True
  Star f <= Star f' = f <= f'
  _ <= Star _ = False
  Symbol _ <= ConsTilde _ _ = True
  Epsilon <= ConsTilde _ _ = True
  Empty <= ConsTilde _ _ = True
  Sum _ _ <= ConsTilde _ _ = True
  Concat _ _ <= ConsTilde _ _ = True
  Star _ <= ConsTilde _ _ = True
  ConsTilde (phi :: BoolForm (Finite n)) es <= ConsTilde (phi' :: BoolForm (Finite n')) es' =
    knownLength es $
      knownLength es' $
        case sameNat (Proxy @n) (Proxy @n') of
          Just p -> (castWith (apply Refl (apply (Refl :: Finite :~: Finite) p)) phi, V.toList es) <= (phi', V.toList es')
          Nothing -> natVal (Proxy @n) <= natVal (Proxy @n')

paren :: [Char] -> [Char]
paren s = "(" ++ s ++ ")"

isSingle :: Exp a -> Bool
isSingle (Symbol _) = True
isSingle Epsilon = True
isSingle Empty = True
isSingle _ = False

instance Show a => Show (Exp a) where
  show (Symbol a) = show a
  show Epsilon = "ε"
  show Empty = "∅"
  show (Sum e1 e2@(Sum _ _)) = show e1 ++ paren (show e2)
  show (Sum e1 e2) = show e1 ++ "+" ++ show e2
  show (Concat e1 e2) = se1 ++ "·" ++ se2
    where
      se1 = case e1 of
        Sum _ _ -> paren $ show e1
        _ -> show e1
      se2 = case e2 of
        Sum _ _ -> paren $ show e2
        Concat _ _ -> paren $ show e2
        _ -> show e2
  show (Star e@(Star _)) = show e ++ "*"
  show (Star e)
    | isSingle e = show e ++ "*"
    | otherwise = paren (show e) ++ "*"
  show (ConsTilde (phi :: BoolForm (Finite n)) es) = "|" ++ show phi ++ "|" ++ "-[" ++ intercalate "," (fmap show (V.toList es)) ++ "]"

instance ToString a => ToString (Exp a) where
  toString (Symbol a) = toString a
  toString Epsilon = "ε"
  toString Empty = "∅"
  toString (Sum e1 e2@(Sum _ _)) = toString e1 ++ paren (toString e2)
  toString (Sum e1 e2) = toString e1 ++ "+" ++ toString e2
  toString (Concat e1 e2) = se1 ++ "·" ++ se2
    where
      se1 = case e1 of
        Sum _ _ -> paren $ toString e1
        _ -> toString e1
      se2 = case e2 of
        Sum _ _ -> paren $ toString e2
        Concat _ _ -> paren $ toString e2
        _ -> toString e2
  toString (Star e@(Star _)) = toString e ++ "*"
  toString (Star e)
    | isSingle e = toString e ++ "*"
    | otherwise = paren (toString e) ++ "*"
  toString (ConsTilde (phi :: BoolForm (Finite n)) es) = "|" ++ toString phi ++ "|[" ++ intercalate "," (fmap toString (V.toList es)) ++ "]"

setConc :: Ord (Exp a) => Set (Exp a) -> Exp a -> Set (Exp a)
setConc _ Empty = S.empty
setConc fs f = S.map (`conc` f) fs

reduceBy :: KnownNat (n + 1) => BoolForm (Finite (n + 1)) -> BoolForm (Finite (n + 1)) -> BoolForm (Finite n)
reduceBy f phi = reduce $ rename (fromMaybe (error "reduceBy: impossible") . unshift) $ subst f (finite 0) phi

reduceBot :: KnownNat (n + 1) => BoolForm (Finite (n + 1)) -> BoolForm (Finite n)
reduceBot = reduceBy Bot

reduceTop :: KnownNat (n + 1) => BoolForm (Finite (n + 1)) -> BoolForm (Finite n)
reduceTop = reduceBy Top

plusComm :: Proxy n -> Proxy m -> n + m :~: m + n
plusComm _ _ = unsafeCoerce Refl

eqVect :: Vector (n + 1) a -> Vector (1 + n) a
eqVect (v :: Vector (n + 1) a) = case plusComm (Proxy @1) (Proxy @n) of Refl -> v

nullAux :: KnownNat n => BoolForm (Finite n) -> Vector n (Exp a) -> Bool
nullAux (phi :: BoolForm (Finite n)) fs =
  case isZero (SNat @n) of
    Zero ->
      ( case reduce phi of
          Top -> True
          _ -> False
      )
    NonZero ->
      nullable (V.head $ eqVect fs) && nullable (ConsTilde (reduceBot phi) (V.tail $ eqVect fs))
        || nullable (ConsTilde (reduceTop phi) (V.tail $ eqVect fs))

nullable :: Exp a -> Bool
nullable Epsilon = True
nullable Empty = False
nullable (Symbol _) = False
nullable (Sum f1 f2) = nullable f1 || nullable f2
nullable (Concat f1 f2) = nullable f1 && nullable f2
nullable (Star _) = True
nullable (ConsTilde phi fs) = knownLength fs $ nullAux phi fs

derivAux :: (Ord (Exp a), Eq a, KnownNat n) => a -> BoolForm (Finite n) -> Vector n (Exp a) -> Set (Exp a)
derivAux a (phi :: BoolForm (Finite n)) fs =
  case isZero (SNat @n) of
    Zero -> S.empty
    NonZero ->
      let f1 = V.head (eqVect fs)
       in if nullable f1
            then
              let fs' = smartCons (reduceBot phi) (V.tail $ eqVect fs)
               in derive a f1 `setConc` fs'
                    `S.union` derive a fs'
                    `S.union` derive a (smartCons (reduceTop phi) (V.tail $ eqVect fs))
            else
              derive a f1 `setConc` smartCons (reduceBot phi) (V.tail $ eqVect fs)
                `S.union` derive a (smartCons (reduceTop phi) (V.tail $ eqVect fs))

derive :: (Ord (Exp a), Eq a) => a -> Exp a -> Set (Exp a)
derive _ Epsilon = S.empty
derive _ Empty = S.empty
derive a (Symbol b)
  | a == b = S.singleton Epsilon
  | otherwise = S.empty
derive a (Sum f1 f2) = derive a f1 `S.union` derive a f2
derive a (Concat f1 f2)
  | nullable f1 = (derive a f1 `setConc` f2) `S.union` derive a f2
  | otherwise = derive a f1 `setConc` f2
derive a f'@(Star f) = derive a f `setConc` f'
derive a (ConsTilde phi fs) = knownLength fs $ derivAux a phi fs

deriveSet :: (Ord (Exp a), Eq a) => a -> Set (Exp a) -> Set (Exp a)
deriveSet a = S.foldl' (\acc e -> derive a e `S.union` acc) S.empty

deriveWord :: (Ord (Exp a), Eq a) => [a] -> Exp a -> Set (Exp a)
deriveWord w e = F.foldl' (flip deriveSet) (S.singleton e) w

deriveBySymbs :: (Ord (Exp a), Eq a) => Set a -> Exp a -> Set (Exp a)
deriveBySymbs as e = F.foldl' (\acc a -> acc `S.union` derive a e) S.empty as

deriveSetsBySymbs :: (Ord (Exp a), Eq a) => Set a -> Set (Exp a) -> Set (Exp a)
deriveSetsBySymbs as = S.foldl' (\acc e -> deriveBySymbs as e `S.union` acc) S.empty

allDeriveBySymbs :: (Ord (Exp a), Eq a) => Set a -> Exp a -> Set (Exp a)
allDeriveBySymbs as e =
  aux S.empty $ S.singleton e
  where
    aux done todo =
      if S.null todo
        then done
        else aux done' todo'
      where
        new = deriveSetsBySymbs as todo
        done' = S.union done todo
        todo' = S.difference new done'

antimirov ::
  (Ord symbol) => Exp symbol -> NFA (Exp symbol) symbol
antimirov e
  | nullable e = setFinal tmp e
  | otherwise = tmp
  where
    tmp = setInitial (computeAccess newNFA $ S.singleton e) e
    sigma = Exp.alphabet e
    computeAccess accuAut toDo
      | S.null toDo = accuAut
      | otherwise = computeAccess newAut newToDo
      where
        (newAut, newToDo) = S.foldr myFunc (accuAut, S.empty) toDo
    myFunc p (accuAut, accuToDo) = S.foldr myFunc2 (accuAut, accuToDo) symbAndDerivs
      where
        symbAndDerivs = S.foldr (\x accu -> accu `S.union` S.map (x,) (derive x p)) S.empty sigma
        myFunc2 (x, e') (newAut, newToDo)
          | e' `isStateIn` newAut = (makeTrans newAut (p, x, e'), newToDo)
          | nullable e' = (makeTrans (addFinalState newAut e') (p, x, e'), S.insert e' newToDo)
          | otherwise = (makeTrans newAut (p, x, e'), S.insert e' newToDo)