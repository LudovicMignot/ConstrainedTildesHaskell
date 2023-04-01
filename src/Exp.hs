{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Exp where

import BoolForm
import Data.Data
import Data.Finite
import Data.Foldable as F
import Data.Maybe
import Data.Set as S
import Data.Singletons.Decide
  ( Decision (Disproved, Proved),
    (%~),
  )
import Data.Vector.Sized as V
import GHC.TypeLits.Singletons
import GHC.TypeNats
import Unsafe.Coerce

-- from https://stackoverflow.com/questions/46634706/haskell-singletons-typelits-package

data IsZero (n :: Nat) where
  Zero :: (0 ~ n) => IsZero n
  NonZero :: (m ~ (n + 1)) => IsZero m

deriving instance Show (IsZero n)

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
nullable (ConsTilde (phi :: BoolForm (Finite n)) fs) = knownLength fs $ nullAux phi fs

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

deriveWord :: (Ord (Exp a), Eq a) => [a] -> Exp a -> Set (Exp a)
deriveWord w e = F.foldl' (\acc a -> S.foldl' (\acc2 e' -> derive a e' `S.union` acc2) S.empty acc) (S.singleton e) w