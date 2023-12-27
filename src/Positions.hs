{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Positions where

import BoolForm (BoolForm, antiAllSat)
import Control.Monad.State.Lazy
import Data.Finite
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector.Sized (Vector, index, knownLength, withSized)
import Exp
import NFA
import ToString

newtype Pos a = Pos (a, Int, [Int]) deriving (Show, Eq, Ord)

addPos :: Int -> Pos a -> Pos a
addPos n (Pos (a, i, ns)) = Pos (a, i, n : ns)

instance ToString a => ToString (Pos a) where
  toString (Pos (a, n, [])) = toString (a, n)
  toString (Pos (a, n, ns)) = toString (a, n, ns)

linearize :: Exp a -> Exp (Pos a)
linearize e = evalState (linearize' e) 1
  where
    linearize' (Symbol a) = do
      n <- get
      put $ n + 1
      return $ Symbol $ Pos (a, n, [])
    linearize' Epsilon = return Epsilon
    linearize' Empty = return Empty
    linearize' (Star e1) = Star <$> linearize' e1
    linearize' (Sum e1 e2) = Sum <$> linearize' e1 <*> linearize' e2
    linearize' (Concat e1 e2) = Concat <$> linearize' e1 <*> linearize' e2
    linearize' (ConsTilde phi es) = ConsTilde phi <$> traverse linearize' es

filterConc :: Int -> [Finite n] -> Vector n (Exp (Pos a)) -> Exp (Pos a)
filterConc _ [] _ = Epsilon
filterConc n pos v = foldl1 Concat $ fmap (fmap (addPos n) . index v) pos

filterSum :: [[Finite n]] -> Vector n (Exp (Pos a)) -> Exp (Pos a)
filterSum [] _ = Empty
filterSum poss v = foldl1 Sum $ (\(i, ps) -> filterConc i ps v) <$> zip [1 ..] poss

devSat :: BoolForm (Finite n) -> Vector n (Exp (Pos a)) -> Exp (Pos a)
devSat phi v = filterSum (knownLength v $ antiAllSat phi) v

pos :: Ord a => Exp (Pos a) -> Set (Pos a)
pos (Symbol s) = Set.singleton s
pos Empty = Set.empty
pos Epsilon = Set.empty
pos (Star e) = pos e
pos (Concat e1 e2) = pos e1 `Set.union` pos e2
pos (Sum e1 e2) = pos e1 `Set.union` pos e2
pos (ConsTilde phi es) = pos $ devSat phi es

first :: Ord a => Exp (Pos a) -> Set (Pos a)
first (Symbol s) = Set.singleton s
first Epsilon = Set.empty
first Empty = Set.empty
first (Star s) = first s
first (Sum e1 e2) = first e1 `Set.union` first e2
first (Concat e1 e2)
  | nullable e1 = first e1 `Set.union` first e2
  | otherwise = first e1
first (ConsTilde phi es) = first $ devSat phi es

last :: Ord a => Exp (Pos a) -> Set (Pos a)
last (Symbol s) = Set.singleton s
last Epsilon = Set.empty
last Empty = Set.empty
last (Star s) = Positions.last s
last (Sum e1 e2) = Positions.last e1 `Set.union` Positions.last e2
last (Concat e1 e2)
  | nullable e2 = Positions.last e1 `Set.union` Positions.last e2
  | otherwise = Positions.last e2
last (ConsTilde phi es) = Positions.last $ devSat phi es

follow :: Ord a => Exp (Pos a) -> Map (Pos a) (Set (Pos a))
follow (Symbol _) = Map.empty
follow Epsilon = Map.empty
follow Empty = Map.empty
follow (Star e) = Map.unionWith Set.union (follow e) $ Map.fromList $ fmap (,first e) (Set.toList $ Positions.last e)
follow (Sum e1 e2) = Map.union (follow e1) $ follow e2
follow (Concat e1 e2) =
  Map.unionsWith Set.union [follow e1, follow e2, Map.fromList $ fmap (,first e2) (Set.toList $ Positions.last e1)]
follow (ConsTilde phi es) = follow $ devSat phi es

data GState a = StateInit | State (Pos a)
  deriving (Eq, Show, Ord)

instance ToString a => ToString (GState a) where
  toString StateInit = "0"
  toString (State p) = toString p

  toHtmlString StateInit = "0"
  toHtmlString (State p) = toHtmlString p

glushkovLin :: Ord a => Exp a -> NFA (GState a) (Pos a)
glushkovLin e = fromLists [StateInit] transitions finals
  where
    e' = linearize e
    null_e = nullable e
    first_e = first e'
    last_e = Positions.last e'
    follow_e = follow e'
    transitions1 = Set.foldr (\p accu -> (StateInit, p, State p) : accu) [] first_e
    transitions =
      Map.foldrWithKey
        ( \p1 followX accu ->
            Set.foldr
              (\p2 accu' -> (State p1, p2, State p2) : accu')
              accu
              followX
        )
        transitions1
        follow_e
    finals
      | null_e = StateInit : Set.toList (Set.map State last_e)
      | otherwise = Set.toList $ Set.map State last_e

glushkov :: (Ord a) => Exp a -> NFA (GState a) a
glushkov e = fromLists [StateInit] transitions $ finalStateList g
  where
    g = glushkovLin e
    transitions = [(x, a, y) | (x, Pos (a, _, _), y) <- transitionList g]