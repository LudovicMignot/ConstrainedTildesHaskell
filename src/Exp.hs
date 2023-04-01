{-# LANGUAGE DataKinds #-}

module Exp where

import BoolForm
import Data.Data
import Data.Finite
import Data.Maybe
import Data.Set as S
import Data.Vector.Sized as V
import GHC.TypeNats

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
