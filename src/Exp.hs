{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import GHC.TypeLits.Singletons
import GHC.TypeNats
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
  show (Sum e1 e2) = show e1 ++ show e2
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

setConc :: Ord (Exp a) => Set (Exp a) -> Exp a -> Set (Exp a)
setConc fs f = S.map (`Concat` f) fs

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
              let fs' = ConsTilde (reduceBot phi) (V.tail $ eqVect fs)
               in derive a f1 `setConc` fs'
                    `S.union` derive a fs'
                    `S.union` derive a (ConsTilde (reduceTop phi) (V.tail $ eqVect fs))
            else
              derive a f1 `setConc` ConsTilde (reduceBot phi) (V.tail $ eqVect fs)
                `S.union` derive a (ConsTilde (reduceTop phi) (V.tail $ eqVect fs))

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
