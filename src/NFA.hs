{-# LANGUAGE ImportQualifiedPost #-}

module NFA where

import Data.Foldable
import Data.GraphViz.Commands
import Data.GraphViz.Types
import Data.GraphViz.Types.Generalised as G
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Lazy (pack)
import ToString

data Config state symbol = Config
  { succs :: Map symbol (Set state),
    finality :: Bool
  }
  deriving (Show)

faToDot ::
  (Ord state, ToString state, ToString symbol) =>
  NFA state symbol ->
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

faToPng :: (Ord state, ToString state, ToString symbol) => FilePath -> NFA state symbol -> IO FilePath
faToPng name auto = addExtension (runGraphviz (parseDotGraph $ pack $ faToDot auto :: G.DotGraph String)) Png name

faToSVG :: (Ord state, ToString state, ToString symbol) => FilePath -> NFA state symbol -> IO FilePath
faToSVG name auto = addExtension (runGraphviz (parseDotGraph $ pack $ faToDot auto :: G.DotGraph String)) Svg name

newEmptyConfig :: Config state symbol
newEmptyConfig = Config {succs = Map.empty, finality = False}

newEmptyFinalConfig :: Config state symbol
newEmptyFinalConfig = Config {succs = Map.empty, finality = True}

newSingleConfig :: symbol -> state -> Config state symbol
newSingleConfig x q = Config {succs = Map.singleton x (Set.singleton q), finality = False}

setFinalConfig :: Config state symbol -> Config state symbol
setFinalConfig conf = conf {finality = True}

data NFA state symbol = NFA
  { initial :: Set state,
    stateConfig :: Map state (Config state symbol)
  }

stateList :: NFA state symbol -> [state]
stateList = Map.keys . stateConfig

finalStateList :: NFA state symbol -> [state]
finalStateList = Map.keys . Map.filter finality . stateConfig

nonFinalStateList :: NFA state symbol -> [state]
nonFinalStateList = Map.keys . Map.filter (not . finality) . stateConfig

transitionList :: NFA c b -> [(c, b, c)]
transitionList NFA {stateConfig = sc} = Map.foldrWithKey myFunc1 [] sc
  where
    myFunc1 p Config {succs = s} accuList =
      Map.foldrWithKey
        ( \x qset accu ->
            Set.foldr (\q accul -> (p, x, q) : accul) accu qset
        )
        accuList
        s

isInitial :: (Ord state) => NFA state symbol -> state -> Bool
isInitial NFA {initial = is} = (`Set.member` is)

isStateIn :: (Ord k) => k -> NFA k symbol -> Bool
isStateIn p = Map.member p . stateConfig

addFinalState :: (Ord state) => NFA state symbol -> state -> NFA state symbol
addFinalState aut@NFA {stateConfig = sc} p =
  aut {stateConfig = Map.insertWith (\_ val -> val) p newEmptyFinalConfig sc}

-- crée les états s'ils n'existent pas
makeTrans :: (Ord state, Ord symbol) => NFA state symbol -> (state, symbol, state) -> NFA state symbol
makeTrans aut@NFA {stateConfig = sc} (p, x, q) =
  aut
    { stateConfig = Map.insertWith (\_ oldval -> oldval) q newEmptyConfig sc'
    }
  where
    sc' = Map.insertWith myFunc p (newSingleConfig x q) sc
    myFunc _ conf@Config {succs = s} = conf {succs = Map.insertWith Set.union x (Set.singleton q) s}

setFinal :: (Ord state) => NFA state symbol -> state -> NFA state symbol
aut@NFA {stateConfig = sc} `setFinal` p =
  aut {stateConfig = Map.insertWith (\_ conf -> setFinalConfig conf) p newEmptyFinalConfig sc}

setInitial :: (Ord state) => NFA state symbol -> state -> NFA state symbol
aut@NFA {stateConfig = sc, initial = is} `setInitial` p =
  aut {initial = Set.insert p is, stateConfig = Map.insertWith (\_ conf -> conf) p newEmptyConfig sc}

newNFA :: NFA state symbol
newNFA = NFA {initial = Set.empty, stateConfig = Map.empty}

instance (Show state, Show symbol, Ord state) => Show (NFA state symbol) where
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
