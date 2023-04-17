-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeFamilies      #-}

module FAClass where

import Data.GraphViz.Commands
import Data.GraphViz.Types
import Data.GraphViz.Types.Generalised as G
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Lazy (pack)
import ToString

class FA fa where
  faToDot ::
    (Ord symbol, Ord state, ToString state, ToString symbol) =>
    fa state symbol ->
    String
  faToDot auto = "digraph{" ++ statementList ++ "}"
    where
      statementList = graphs ++ nodes ++ edges
      graphs = "graph [rankdir = LR];\n"
      nodes = finals ++ nonFinals
      finals = concatMap (\s -> myToString s ++ att1 s) $ finalStateList auto
      nonFinals = concatMap (\s -> myToString s ++ att2 s) $ nonFinalStateList auto
      edges = concatMap (\(p, x, q) -> myToString p ++ "->" ++ myToString q ++ " [label = <" ++ toHtmlString x ++ ">];\n") trans
      trans =
        Map.foldlWithKey' (\accu (p, q) x -> (p, x, q) : accu) [] $
          foldl' (\accu (p, x, q) -> Map.insertWith (++) (p, q) [x] accu) Map.empty $
            transitionList auto
      myToString p = "\"" ++ toHtmlCapString p ++ "\""
      att1 p
        | isInitial auto p =
            " [shape = octagon, peripheries = 2, style = rounded, style = filled, color = gray35" ++ ", label=<" ++ toHtmlString p ++ ">];\n"
        | otherwise =
            " [shape = box, peripheries = 2, style = rounded" ++ ", label=<" ++ toHtmlString p ++ ">];\n"
      att2 p
        | isInitial auto p =
            " [shape = octagon, style = rounded, style = filled, color = gray35" ++ ", label=<" ++ toHtmlString p ++ ">];\n"
        | otherwise =
            " [shape = box, style = rounded" ++ ", label=<" ++ toHtmlString p ++ ">];\n"

  faToPng :: (Ord symbol, Ord state, ToString state, ToString symbol) => String -> fa state symbol -> IO FilePath
  faToPng name auto = addExtension (runGraphviz (parseDotGraph $ pack $ faToDot auto :: G.DotGraph String)) Png name

  newFA :: state -> fa state symbol

  fromLists ::
    (Ord state, Ord symbol) =>
    state ->
    [(state, symbol, state)] ->
    [state] ->
    fa state symbol
  fromLists i transList finalList = res
    where
      a1 = newFA i
      a2 = foldr (flip makeTrans) a1 transList
      res = foldr (flip setFinal) a2 finalList

  alphabet :: (Ord symbol) => fa state symbol -> Set symbol

  stateList :: (Ord state) => fa state symbol -> [state]
  stateList = Set.toList . stateSet

  stateSet :: (Ord state) => fa state symbol -> Set state
  stateSet = Set.fromList . stateList

  initialStateList :: (Ord state) => fa state symbol -> [state]
  initialStateList aut = filter (isInitial aut) (stateList aut)

  initialStateSet :: (Ord state) => fa state symbol -> Set state
  initialStateSet = Set.fromList . initialStateList

  finalStateList :: (Ord state) => fa state symbol -> [state]
  finalStateList = Set.toList . finalStateSet

  finalStateSet :: (Ord state) => fa state symbol -> Set state
  finalStateSet = Set.fromList . finalStateList

  nonFinalStateList :: (Ord state) => fa state symbol -> [state]
  nonFinalStateList = Set.toList . nonFinalStateSet

  nonFinalStateSet :: (Ord state) => fa state symbol -> Set state
  nonFinalStateSet = Set.fromList . nonFinalStateList

  transitionListFrom ::
    (Ord state, Ord symbol) =>
    fa state symbol ->
    state ->
    [(state, symbol, state)]

  transitionList :: (Ord state, Ord symbol) => fa state symbol -> [(state, symbol, state)]
  transitionList = Set.toList . transitionSet

  transitionSet :: (Ord state, Ord symbol) => fa state symbol -> Set (state, symbol, state)
  transitionSet = Set.fromList . transitionList

  forAllTransitionsDo :: (Ord state, Ord symbol) => ((state, symbol, state) -> c -> c) -> c -> fa state symbol -> c
  forAllTransitionsDo f accu aut = foldr f accu (transitionList aut)

  forAllStatesDo ::
    (Ord state, Ord symbol) =>
    (state -> c -> c) ->
    c ->
    fa state symbol ->
    c
  forAllStatesDo f accu aut = Set.foldr f accu (stateSet aut)

  forAllFinalStatesDo :: (Ord state, Ord symbol) => (state -> c -> c) -> c -> fa state symbol -> c
  forAllFinalStatesDo f accu aut = Set.foldr f accu (finalStateSet aut)

  forAllNonFinalStatesDo :: (Ord state, Ord symbol) => (state -> c -> c) -> c -> fa state symbol -> c
  forAllNonFinalStatesDo f accu aut = Set.foldr f accu (nonFinalStateSet aut)

  filterState :: (Ord state) => (state -> Bool) -> fa state symbol -> fa state symbol

  isFinal :: (Ord state) => fa state symbol -> state -> Bool

  isInitial :: (Ord state) => fa state symbol -> state -> Bool

  maxState :: Ord state => fa state symbol -> Maybe state

  freeState :: (Enum state, Ord state, Bounded state) => fa state symbol -> state
  freeState aut
    | isNothing m = minBound
    | otherwise = succ $ fromJust m
    where
      m = maxState aut

  oneSuccOfBy ::
    (Ord state, Ord symbol) =>
    fa state symbol ->
    state ->
    symbol ->
    Maybe state

  succListOf ::
    (Ord state) =>
    fa state symbol ->
    state ->
    [state]
  succListOf aut = Set.toList . succSetOf aut

  succSetOf ::
    (Ord state) =>
    fa state symbol ->
    state ->
    Set state
  succSetOf aut = Set.fromList . succListOf aut

  isStateIn :: (Ord state) => state -> fa state symbol -> Bool
  isStateIn p aut = Set.member p (stateSet aut)

  recognizes ::
    (Ord state, Ord symbol) =>
    fa state symbol ->
    [symbol] ->
    Bool

  isSuccStateOf ::
    (Ord state) =>
    state ->
    state ->
    fa state symbol ->
    Bool
  isSuccStateOf p q aut = elem q $ succListOf aut p

  addState ::
    (Ord state) =>
    fa state symbol ->
    state ->
    fa state symbol

  addNewState ::
    (Enum state, Ord state, Bounded state) =>
    fa state symbol ->
    (state, fa state symbol)
  addNewState a = (new, a `addState` new)
    where
      new = freeState a

  addFinalState ::
    (Ord state) =>
    fa state symbol ->
    state ->
    fa state symbol

  addNewFinalState ::
    (Enum state, Ord state, Bounded state) =>
    fa state symbol ->
    (state, fa state symbol)
  addNewFinalState a = (new, a `addFinalState` new)
    where
      new = freeState a

  makeTrans ::
    (Ord state, Ord symbol) =>
    fa state symbol ->
    (state, symbol, state) ->
    fa state symbol

  setFinal ::
    (Ord state) =>
    fa state symbol ->
    state ->
    fa state symbol

  setNonFinal ::
    (Ord state) =>
    fa state symbol ->
    state ->
    fa state symbol

  setAllFinal ::
    (Ord state, Ord symbol) =>
    fa state symbol ->
    fa state symbol
  setAllFinal aut = forAllNonFinalStatesDo (flip setFinal) aut aut

  setAllNonFinal ::
    (Ord state, Ord symbol) =>
    fa state symbol ->
    fa state symbol
  setAllNonFinal aut = forAllFinalStatesDo (flip setNonFinal) aut aut

  addNewSucc ::
    (Ord symbol, Enum state, Ord state, Bounded state) =>
    fa state symbol ->
    (state, symbol) ->
    (state, fa state symbol)
  addNewSucc auto (p, a) = (new, auto `makeTrans` (p, a, new))
    where
      new = freeState auto

  addNewFinalSucc ::
    (Ord symbol, Enum state, Ord state, Bounded state) =>
    fa state symbol ->
    (state, symbol) ->
    (state, fa state symbol)
  addNewFinalSucc auto (p, a) = (new, auto' `makeTrans` (p, a, new))
    where
      (new, auto') = addNewFinalState auto

  addNewClone ::
    (Ord symbol, Enum state, Ord state, Bounded state) =>
    fa state symbol ->
    state ->
    Maybe (state, fa state symbol)

  addNewFinalClone ::
    (Ord symbol, Enum state, Ord state, Bounded state) =>
    fa state symbol ->
    state ->
    Maybe (state, fa state symbol)
  addNewFinalClone aut p
    | isNothing maybeAddClone = Nothing
    | otherwise = Just (new, setFinal auto new)
    where
      maybeAddClone = addNewClone aut p
      Just (new, auto) = maybeAddClone

  setInitial ::
    (Ord state) =>
    fa state symbol ->
    state ->
    fa state symbol

  accessibleStates :: (Ord state) => fa state symbol -> Set state

  coAccessibleStates :: (Ord state, Ord symbol) => fa state symbol -> Set state

  usefullStates :: (Ord state, Ord symbol) => fa state symbol -> Set state
  usefullStates aut = Set.intersection (accessibleStates aut) (coAccessibleStates aut)

  setAccessible ::
    (Ord state, Ord symbol) =>
    fa state symbol ->
    fa state symbol
  setAccessible aut = filterState (`Set.member` access) aut
    where
      access = accessibleStates aut

  setCoAccessible ::
    (Ord state, Ord symbol) =>
    fa state symbol ->
    fa state symbol
  setCoAccessible aut = filterState (`Set.member` coaccess) aut
    where
      coaccess = coAccessibleStates aut

  trim ::
    (Ord state, Ord symbol) =>
    fa state symbol ->
    fa state symbol
  trim aut = filterState (`Set.member` usefull) aut
    where
      usefull = usefullStates aut

  prefixes ::
    (Ord state, Ord symbol) =>
    fa state symbol ->
    fa state symbol
  prefixes = setAllFinal . trim

  concChar ::
    (Ord symbol, Enum state, Ord state, Bounded state) =>
    fa state symbol ->
    symbol ->
    fa state symbol
  concChar aut x = forAllFinalStatesDo myFunc (setAllNonFinal aut) aut
    where
      myFunc p accu
        | isNothing maybeQ = makeTrans aut' (p, x, new)
        | otherwise = makeTrans aut2 (p, x, new2)
        where
          maybeQ = oneSuccOfBy aut p x
          (new, aut') = addNewFinalState accu
          Just q = maybeQ
          Just (new2, aut2) = addNewFinalClone accu q

  standardiser ::
    (Ord symbol, Enum state, Ord state, Bounded state) =>
    fa state symbol ->
    fa state symbol
  standardiser aut = setInitial aut'' i'
    where
      initials = initialStateList aut
      transFromInits = foldr (\i accu -> transitionListFrom aut i ++ accu) [] initials
      (i', aut')
        | recognizes aut [] = addNewFinalState aut
        | otherwise = addNewState aut
      aut'' = foldr (\(_, x, q) accuAut -> makeTrans accuAut (i', x, q)) aut' transFromInits

  renameViaFun ::
    (Ord state, Ord state') =>
    fa state symbol ->
    (state -> state') ->
    fa state' symbol

  renameViaMap ::
    (Ord state, Ord state') =>
    fa state symbol ->
    Map state state' ->
    fa state' symbol
  renameViaMap aut m = renameViaFun aut (m Map.!)

  renameStates ::
    (Ord state, Enum state', Bounded state', Ord state') =>
    fa state symbol ->
    fa state' symbol
  renameStates aut = renameViaMap aut renaming
    where
      renaming = Map.fromList $ zip (stateList aut) [minBound ..]

  renameStatesToWords :: (Ord state) => fa state symbol -> fa Word symbol
  renameStatesToWords aut = renameViaMap aut renaming
    where
      renaming = Map.fromList $ zip (stateList aut) [0 :: Word ..]

  allWordsOfLengthSmallerThan ::
    (Enum state, Bounded state, Ord state, Ord symbol) =>
    Word ->
    [symbol] ->
    fa state symbol
  allWordsOfLengthSmallerThan k sigma = renameStates $ fromLists 0 transList [0 .. k]
    where
      transList = [(p, x, p + 1) | p <- [0 .. k], x <- sigma]
