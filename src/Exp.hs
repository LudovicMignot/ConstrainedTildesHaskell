{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Exp where

import BoolForm
import Data.Data
import Data.Finite
import Data.Maybe
import Data.Set as S
import Data.Vector.Sized as V
-- import GHC.TypeLits.Singletons
import GHC.TypeNats
import Unsafe.Coerce

class NatSingleton (p :: Nat -> *) where
  natSingleton :: KnownNat n => p n

-- | A natural number is either 0 or 1 plus something.
data NatIsZero (n :: Nat) where
  IsZero :: NatIsZero 0
  IsNonZero :: KnownNat n => NatIsZero (1 + n)

instance NatSingleton NatIsZero where
  natSingleton :: forall n. KnownNat n => NatIsZero n
  natSingleton = case natVal (Proxy :: Proxy n) of
    0 -> (unsafeCoerce :: NatIsZero 0 -> NatIsZero n) IsZero
    n -> case someNatVal (n - 1) of
      (SomeNat (p :: Proxy m)) -> (unsafeCoerce :: NatIsZero (1 + m) -> NatIsZero n) $ IsNonZero

data NatPeano (n :: Nat) where
  PeanoZero :: NatPeano 0
  PeanoSucc :: KnownNat n => NatPeano n -> NatPeano (1 + n)

deriving instance Show (NatPeano n)

instance NatSingleton NatPeano where
  natSingleton :: forall n. KnownNat n => NatPeano n
  natSingleton = case natSingleton :: NatIsZero n of
    IsZero -> PeanoZero
    IsNonZero -> PeanoSucc natSingleton

data Exp a where
  Symbol :: a -> Exp a
  Epsilon :: Exp a
  Empty :: Exp a
  Sum :: (Exp a) -> (Exp a) -> Exp a
  Concat :: (Exp a) -> (Exp a) -> Exp a
  Star :: (Exp a) -> Exp a
  ConsTilde :: KnownNat n => (BoolForm (Finite n)) -> (Vector n (Exp a)) -> Exp a

setConc :: Ord (Exp a) => Set (Exp a) -> Exp a -> Set (Exp a)
setConc fs f = S.map (`Concat` f) fs

reduceBy :: KnownNat (n + 1) => BoolForm (Finite (n + 1)) -> BoolForm (Finite (n + 1)) -> BoolForm (Finite n)
reduceBy f phi = reduce $ rename (fromMaybe (error "reduceBy: impossible") . unshift) $ subst f (finite 0) phi

reduceBot :: KnownNat (n + 1) => BoolForm (Finite (n + 1)) -> BoolForm (Finite n)
reduceBot = reduceBy Bot

reduceTop :: KnownNat (n + 1) => BoolForm (Finite (n + 1)) -> BoolForm (Finite n)
reduceTop = reduceBy Top

nullAux :: KnownNat n => BoolForm (Finite n) -> Vector n (Exp a) -> Proxy n -> Bool
nullAux phi fs (p :: Proxy n) =
  case (natSingleton :: NatPeano n) of
    PeanoZero ->
      ( case reduce phi of
          Top -> True
          _ -> False
      )
    PeanoSucc k ->
      nullable (V.head fs) && nullable (ConsTilde (reduceBot phi) (V.tail fs))
        || nullable (ConsTilde (reduceTop phi) (V.tail fs))

nullable :: Exp a -> Bool
nullable Epsilon = True
nullable Empty = False
nullable (Symbol _) = False
nullable (Sum f1 f2) = nullable f1 || nullable f2
nullable (Concat f1 f2) = nullable f1 && nullable f2
nullable (Star _) = True
nullable (ConsTilde phi fs) =
  if V.length fs == 0
    then case reduce phi of
      Top -> True
      _ -> False
    else
      nullable (V.head fs) && nullable (ConsTilde (reduceBot phi) (V.tail fs))
        || nullable (ConsTilde (reduceTop phi) (V.tail fs))

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
derive a (ConsTilde phi fs) = error "to do"
