module NFA where

import Data.Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import FAClass

data Config state symbol = Config
  { succs :: Map symbol (Set state),
    finality :: Bool
  }
  deriving (Show)

newEmptyConfig :: Config state symbol
newEmptyConfig = Config {succs = Map.empty, finality = False}

newEmptyFinalConfig :: Config state symbol
newEmptyFinalConfig = Config {succs = Map.empty, finality = True}

newSingleConfig :: (Ord symbol, Ord state) => symbol -> state -> Config state symbol
newSingleConfig x q = Config {succs = Map.singleton x (Set.singleton q), finality = False}

setFinalConfig :: Config state symbol -> Config state symbol
setFinalConfig conf = conf {finality = True}

setNonFinalConfig :: Config state symbol -> Config state symbol
setNonFinalConfig conf = conf {finality = False}

data NFA state symbol = NFA
  { initial :: Set state,
    stateConfig :: Map state (Config state symbol)
  }

instance FA NFA where
  initialStateList = Set.toList . initial

  newFA i = NFA {initial = Set.singleton i, stateConfig = Map.singleton i newEmptyConfig}

  alphabet NFA {stateConfig = sc} = Set.fromList alphabetList
    where
      alphabetList = Map.foldr myFunc [] sc
      myFunc conf accu = Map.keys (succs conf) ++ accu

  stateList = Map.keys . stateConfig

  finalStateList = Map.keys . Map.filter finality . stateConfig

  nonFinalStateList = Map.keys . Map.filter (not . finality) . stateConfig

  transitionListFrom NFA {stateConfig = sc} p =
    fromMaybe [] $
      Map.lookup p sc
        >>= ( \Config {succs = s} ->
                Just $
                  Map.foldrWithKey
                    (\x qset accu -> Set.foldr (\q accul -> (p, x, q) : accul) accu qset)
                    []
                    s
            )

  transitionList NFA {stateConfig = sc} = Map.foldrWithKey myFunc1 [] sc
    where
      myFunc1 p Config {succs = s} accuList =
        Map.foldrWithKey
          ( \x qset accu ->
              Set.foldr (\q accul -> (p, x, q) : accul) accu qset
          )
          accuList
          s

  isFinal aut p = Set.member p $ finalStateSet aut

  isInitial NFA {initial = is} = (`Set.member` is)

  maxState NFA {stateConfig = sc} =
    Map.foldrWithKey myFunc Nothing sc
    where
      myFunc p _ (Just accu) = Just $ max p accu
      myFunc p _ _ = Just p

  oneSuccOfBy NFA {stateConfig = sc} p x =
    Map.lookup p sc >>= (\Config {succs = s} -> Map.lookup x s >>= find (const True))

  succSetOf NFA {stateConfig = sc} p =
    fromMaybe Set.empty $ Map.lookup p sc >>= Just . Set.unions . Map.elems . succs

  isStateIn p = Map.member p . stateConfig

  aut `recognizes` w = any (isFinal aut) $ (aut, w) `sendsStateSet` initial aut

  addState aut@NFA {stateConfig = sc} p =
    aut {stateConfig = Map.insertWith (\_ val -> val) p newEmptyConfig sc}

  addFinalState aut@NFA {stateConfig = sc} p =
    aut {stateConfig = Map.insertWith (\_ val -> val) p newEmptyFinalConfig sc}

  -- crée les états s'ils n'existent pas
  makeTrans aut@NFA {stateConfig = sc} (p, x, q) =
    aut
      { stateConfig = Map.insertWith (\_ oldval -> oldval) q newEmptyConfig sc'
      }
    where
      sc' = Map.insertWith myFunc p (newSingleConfig x q) sc
      myFunc _ conf@Config {succs = s} = conf {succs = Map.insertWith Set.union x (Set.singleton q) s}

  aut@NFA {stateConfig = sc} `setFinal` p =
    aut {stateConfig = Map.insertWith (\_ conf -> setFinalConfig conf) p newEmptyFinalConfig sc}

  aut@NFA {stateConfig = sc} `setNonFinal` p =
    aut {stateConfig = Map.insertWith (\_ conf -> setNonFinalConfig conf) p newEmptyConfig sc}

  addNewClone aut p
    | isNothing maybesc' = Nothing
    | otherwise = Just (new, auto {stateConfig = sc'})
    where
      (new, auto@NFA {stateConfig = sc}) = addNewState aut
      maybesc' = Map.lookup p sc >>= (\conf -> Just $ Map.insert new conf sc)
      Just sc' = maybesc'

  aut@NFA {stateConfig = sc, initial = is} `setInitial` p =
    aut {initial = Set.insert p is, stateConfig = Map.insertWith (\_ conf -> conf) p newEmptyConfig sc}

  accessibleStates aut@NFA {initial = is} = accessFrom Set.empty is
    where
      accessFrom accuSet toDo
        | Set.null toDo = accuSet
        | otherwise = accessFrom accuSet' toDo'
        where
          (accuSet', toDo') = Set.foldr myFunc (accuSet, Set.empty) toDo
          myFunc p (dejaVu, aTraiter)
            | Set.member p dejaVu = (dejaVu, aTraiter)
            | otherwise = (Set.insert p dejaVu, Set.union aTraiter (succSetOf aut p))

  filterState f NFA {stateConfig = sc, initial = is} =
    NFA {initial = Set.filter f is, stateConfig = Map.foldrWithKey myFunc Map.empty sc}
    where
      myFunc p (Config s b) accu
        | not $ f p = accu
        | otherwise = Map.insert p (Config (Map.map (Set.filter f) s) b) accu

  coAccessibleStates = accessibleStates . NFA.reverse

  setAccessible aut@NFA {stateConfig = sc} =
    aut {stateConfig = Map.filterWithKey (\p _ -> isAccess p) sc}
    where
      access = accessibleStates aut
      isAccess p = Set.member p access

  setCoAccessible = NFA.reverse . setAccessible . NFA.reverse

  trim = NFA.reverse . setAccessible . NFA.reverse . setAccessible

  renameViaFun aut@NFA {initial = i, stateConfig = sc} renaming =
    aut {initial = i', stateConfig = sc'}
    where
      i' = Set.map renaming i
      sc' = Map.foldrWithKey myFunc2 Map.empty sc
      myFunc2 p confOfP =
        Map.insert (renaming p) (rename confOfP)
      rename conf@Config {succs = s} = conf {succs = s'}
        where
          s' = Map.map (Set.map renaming) s

newNFA :: NFA state symbol
newNFA = NFA {initial = Set.empty, stateConfig = Map.empty}

fromListsNFA ::
  (Ord state, Eq state, Eq symbol, Ord symbol) =>
  [state] ->
  [(state, symbol, state)] ->
  [state] ->
  NFA state symbol
fromListsNFA initialList transList finalList = res
  where
    a1 = newNFA
    a2 = foldr (flip makeTrans) a1 transList
    a3 = foldr (flip setFinal) a2 finalList
    res = foldr (flip setInitial) a3 initialList

succOfBy ::
  (Eq state, Ord state, Eq symbol, Ord symbol) =>
  NFA state symbol ->
  (state, symbol) ->
  Set state
succOfBy NFA {stateConfig = sc} (p, x) =
  fromMaybe Set.empty $ Map.lookup p sc >>= Map.lookup x . succs

sendsStateSet ::
  (Eq state, Ord state, Eq symbol, Ord symbol) =>
  (NFA state symbol, [symbol]) ->
  Set state ->
  Set state
(auto, word) `sendsStateSet` ps = foldr myFunc ps word
  where
    myFunc x =
      Set.foldr (\q accu -> Set.union accu $ auto `succOfBy` (q, x)) Set.empty

sendsState ::
  (Eq state, Ord state, Eq symbol, Ord symbol) =>
  (NFA state symbol, [symbol]) ->
  state ->
  Set state
(auto, word) `sendsState` p = (auto, word) `sendsStateSet` Set.singleton p

reverse :: (Ord state, Ord symbol, Eq state, Eq symbol, FA fa) => fa state symbol -> NFA state symbol
reverse aut = fromListsNFA fs (map (\(p, x, q) -> (q, x, p)) ts) is
  where
    is = initialStateList aut
    fs = finalStateList aut
    ts = transitionList aut

shuffle ::
  (FA fa, Eq state, Eq state', Eq symbol, Ord state, Ord state', Ord symbol) =>
  fa state symbol ->
  fa state' symbol ->
  NFA (state, state') symbol
shuffle a1 a2 = fromListsNFA inits trans finals
  where
    inits = [(i1, i2) | i1 <- initialStateList a1, i2 <- initialStateList a2]
    trans =
      [((p1, p2), x, (q1, p2)) | (p1, x, q1) <- transitionList a1, p2 <- stateList a2]
        ++ [((p1, p2), x, (p1, q2)) | (p2, x, q2) <- transitionList a2, p1 <- stateList a1]
    finals = [(p, q) | p <- finalStateList a1, q <- finalStateList a2]

instance (Show state, Show symbol, Eq state, Ord state) => Show (NFA state symbol) where
  show aut@NFA {initial = i} =
    Map.foldrWithKey myFunc "" $ stateConfig aut
    where
      myFunc p confOfP accu =
        ifInit p ++ final p confOfP ++ trans confOfP ++ "\n" ++ accu
      ifInit p
        | Set.member p i = "->"
        | otherwise = ""
      final p confOfP
        | finality confOfP = show p ++ "**"
        | otherwise = show p
      trans Config {succs = s}
        | Map.null s = ""
        | otherwise =
            '[' : Map.foldrWithKey (\x qs accu -> Set.foldr (\q accu' -> show (x, q) ++ accu') accu qs) "]" s
