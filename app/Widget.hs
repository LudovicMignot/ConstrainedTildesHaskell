{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Widget where

import ArbitraryExp
import Control.Monad (join, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString as BS
import Data.GraphViz.Commands
  ( GraphvizCommand (Dot),
    GraphvizOutput (Svg),
    graphvizWithHandle,
  )
import Data.GraphViz.Types (parseDotGraph)
import Data.GraphViz.Types.Generalised as G (DotGraph)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text as Te
  ( Text,
    pack,
    unpack,
  )
import qualified Data.Text.Lazy as Tel
import Exp (Exp (Epsilon))
import ExpFromString (expFromString)
import NFA (Config (Config, succs), NFA (..), faToDot, stateList)
import Reflex.Dom.Core
  ( EventName (Click),
    HasDomEvent (domEvent),
    MonadHold (holdDyn),
    MonadWidget,
    PerformEvent (performEvent),
    Reflex (Dynamic, Event),
    TextInput (_textInput_value),
    blank,
    constDyn,
    def,
    dyn,
    el,
    elAttr,
    elAttr',
    elDynHtml',
    elDynHtmlAttr',
    ffor,
    leftmost,
    text,
    textInput,
    textInputConfig_attributes,
    textInputConfig_initialValue,
    textInputConfig_inputType,
    (&),
    (.~),
    (=:),
  )
import ToString (ToString (toHtmlString, toString))

diceButton :: (MonadWidget t m) => m (Event t ())
diceButton = do
  (e, _) <- elAttr' "div" ("class" =: "btn btn-primary mx-1") $ text "⚃"
  return $ () <$ domEvent Click e

data Method = Ant | Glu -- add  | Fol

lecteurExp :: (MonadWidget t m) => m (Dynamic t (Maybe (Exp Char)))
lecteurExp = el "form" $
  elAttr "div" ("class" =: "form-group") $ do
    elAttr "label" ("for" =: "inputExp") $ text "Expression"

    res <- elAttr "div" ("class" =: "input-group") $ do
      rec d_t <- holdDyn (Just "(a+b)*.a.(a+b)") evt2
          express <- dyn $ expInput "inputExp" <$> d_t
          let eDef = expFromString "(a+b)*.a.(a+b)"
          evt <- diceButton
          helpButton "modal1" "How To" $ do
            el "ul" $ do
              el "li" $ text "Enter an extended expression over the symbols {a,...,z}."
              el "li" $ text "Authorized Boolean operators are {∧, ¬, ∨, ⊥, ⊤}."
              el "li" $ text "Authorized Boolean atoms are {0, 1, ...}."
              el "li" $ text "Authorized expression operators are {+, ., *, ε, ∅}."
              el "li" $ text "Boolean formulae for tildes must be enclosed in \"|\" "
              el "li" $ text "Then choose a construction method and click the button."
            return ()
          evt2 <-
            performEvent $
              ffor evt $
                const $
                  liftIO $
                    fmap (Te.pack . toString)
                      <$> (Just <$> getAlea 5)
      join <$> holdDyn (constDyn eDef) express
    elAttr "small" ("class" =: "form-text text-muted") $
      text
        "Enter an extended expression."
    return res

grpBout :: (MonadWidget t m) => m (Dynamic t (Maybe Method))
grpBout =
  elAttr
    "div"
    ( Map.fromList
        [ ( "style",
            "display : flex; flex-direction : row; flex-wrap : wrap; justify-content : center; align-items : center; align-content : center;"
          )
        ]
    )
    $ do
      (e1, _) <-
        elAttr' "button" ("class" =: "btn btn-primary mx-1") $
          text "Glushkov"
      (e2, _) <-
        elAttr' "button" ("class" =: "btn btn-primary mx-1") $
          text "Antimirov"
      -- (e3, _) <-
      --   elAttr' "button" ("class" =: "btn btn-primary mx-1") $
      --     text "Follow"

      let clicks1 = Just Glu <$ domEvent Click e1
      let clicks2 = Just Ant <$ domEvent Click e2
      -- let clicks3 = const (Just Fol) <$> domEvent Click e2

      holdDyn Nothing $ leftmost [clicks1, clicks2] -- old clicks3, clicks4, clicks5]

helpButton :: (MonadWidget t m) => Text -> Text -> m () -> m ()
helpButton ident title content = do
  elAttr
    "button"
    ( Map.fromList
        [ ("type", "button"),
          ("class", "btn btn-primary rounded-circle ml-2"),
          ("data-toggle", "modal"),
          ("data-target", mappend "#" ident)
        ]
    )
    $ text "?"
  --
  elAttr
    "div"
    ( Map.fromList
        [ ("class", "modal fade"),
          ("id", ident),
          ("data-toggle", "modal"),
          ("data-target", mappend "#" ident)
        ]
    )
    $ elAttr "div" ("class" =: "modal-dialog modal-dialog-centered") $
      elAttr "div" ("class" =: "modal-content") $
        do
          _ <- elAttr "div" ("class" =: "modal-header") $ do
            elAttr "h5" ("class" =: "modal-title") $ text title
            elAttr
              "button"
              ( Map.fromList
                  [ ("type", "button"),
                    ("class", "close"),
                    ("data-dismiss", "modal"),
                    ("data-target", mappend "#" ident)
                  ]
              )
              $ elDynHtml' "span" $ constDyn "&times"
          elAttr "div" ("class" =: "modal-body") content
          elAttr "div" ("class" =: "modal-footer") $
            elAttr
              "button"
              ( Map.fromList
                  [ ("type", "button"),
                    ("class", "btn btn-secondary"),
                    ("data-dismiss", "modal")
                  ]
              )
              $ text "Close"

expInput ::
  (MonadWidget t m) =>
  Text ->
  Maybe Text ->
  m (Dynamic t (Maybe (Exp Char)))
expInput ident start = do
  let errorState =
        Map.fromList [("class", "form-control is-invalid"), ("id", ident)]
      validState =
        Map.fromList [("class", "form-control is-valid"), ("id", ident)]
  rec n <-
        textInput $
          def
            & textInputConfig_inputType
            .~ "text"
            & textInputConfig_initialValue
            .~ fromMaybe "(a+b)*.a.(a+b)" start
            & textInputConfig_attributes
            .~ attrs

      let result = expFromString . Te.unpack <$> _textInput_value n
          attrs = fmap (maybe errorState (const validState)) result
  return result

svgAut ::
  ( MonadWidget t m,
    ToString symbol,
    ToString state,
    Ord state
  ) =>
  NFA state symbol ->
  m ()
svgAut auto = do
  let getData handle = do
        bytes <- BS.hGetContents handle
        return $ Te.pack $ toString bytes
  svg <- liftIO $ graphvizWithHandle Dot (parseDotGraph $ Tel.pack $ faToDot auto :: G.DotGraph String) Svg getData
  void $ elDynHtml' "div" $ return svg

renameViaFun ::
  (Ord state') =>
  NFA state symbol ->
  (state -> state') ->
  NFA state' symbol
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

renameViaMap ::
  (Ord state, Ord state') =>
  NFA state symbol ->
  Map state state' ->
  NFA state' symbol
renameViaMap aut m = renameViaFun aut (m Map.!)

svgAutWithMap ::
  ( MonadWidget t m,
    ToString state,
    Ord state,
    ToString symbol
  ) =>
  NFA state symbol ->
  m ()
svgAutWithMap auto = do
  let thestates = Prelude.zip (stateList auto) [1 :: Word ..]
  let themap = Map.fromList thestates
  let auto' = renameViaMap auto themap
  svgAut auto'
  let info =
        Prelude.map
          (\(s, i) -> (Te.pack $ show i ++ " = ", Te.pack $ toHtmlString s))
          $ Map.toList themap
  el "table" $ tableDraw info

tableDraw :: (MonadWidget t m) => [(Text, Text)] -> m ()
tableDraw [] = blank
tableDraw [(x, fols)] = el "tr" $ myDraw x fols
tableDraw ((x1, fols1) : (x2, fols2) : reste) = do
  el "tr" $ do
    myDraw x1 fols1
    myDraw x2 fols2
  tableDraw reste

myDraw :: (MonadWidget t m) => Text -> Text -> m ()
myDraw x fols = do
  void $ elDynHtmlAttr' "td" ("class" =: "text-right pl-3") $ constDyn x
  void $ elDynHtmlAttr' "td" ("class" =: "text-left pr-3") $ constDyn fols
