{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Widget where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.IO.Class
-- import qualified Data.HashMap.Strict as HMap

import Data.Hashable
import Data.JSString as JS
import Data.Map qualified as Map
import Data.Map.Strict
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text as Te
  ( Text,
    pack,
    unpack,
  )
import Exp
import ExpFromString
import NFA
import Reflex
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.Class.Events
import Reflex.Dom.Core
import Reflex.Dom.Widget.Basic
import Reflex.Dom.Widget.Input
import ToString

-- import Data.HashMap.Strict

diceButton :: (MonadWidget t m) => m (Event t ())
diceButton = do
  (e, _) <- elAttr' "div" ("class" =: "btn btn-primary mx-1") $ text "⚃"
  return $ () <$ domEvent Click e

data Method = Ant -- add | Glu | Brzo | Cont | Fol

randomExpr :: Integer -> Set Char -> IO (Maybe (Exp Char))
randomExpr _ _ = return $ Just Epsilon

lecteurExp :: (MonadWidget t m) => m (Dynamic t (Maybe (Exp Char)))
lecteurExp = el "form" $
  elAttr "div" ("class" =: "form-group") $ do
    elAttr "label" ("for" =: "inputExp") $ text "Expression"
    -- res <- elAttr "div" ("class" =: "input-group") $ do
    --   expression <- expInput "inputExp"
    --   --
    --   helpButton "modal1" "How To" $ do
    --     el "ul" $ do
    --       el "li" $ text "Enter a regular expression over the symbols {a,...,z}."
    --       el "li" $ text "Authorized operators are {+, ., *, 1, 0}."
    --       el "li" $ text "Then choose a construction method and click the button."
    --     return ()
    --   --
    --   return expression

    res <- elAttr "div" ("class" =: "input-group") $ do
      rec d_t <- holdDyn (Just "(a+b)*.a.(a+b)") evt2

          express <- dyn $ expInput "inputExp" <$> d_t

          let eDef = expFromString "(a+b)*.a.(a+b)"

          evt <- diceButton

          let symboles = Set.fromList ['a', 'b', 'c']

          evt2 <-
            performEvent $
              ffor evt $
                const $
                  liftIO $
                    fmap (Te.pack . toString)
                      <$> randomExpr 5 symboles
      join <$> holdDyn (constDyn eDef) express
    elAttr "small" ("class" =: "form-text text-muted") $
      text
        "Enter a regular expression, made of the symbols in {a,..., z, +, ., *, 0, 1}."
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
      -- (e1, _) <-
      --   elAttr' "button" ("class" =: "btn btn-primary mx-1") $
      --     text "Glushkov"
      (e2, _) <-
        elAttr' "button" ("class" =: "btn btn-primary mx-1") $
          text "Antimirov"
      -- (e3, _) <-
      --   elAttr' "button" ("class" =: "btn btn-primary mx-1") $
      --     text "Follow"
      -- (e4, _) <-
      --   elAttr' "button" ("class" =: "btn btn-primary mx-1") $
      --     text "Brzozowski"
      -- (e5, _) <-
      --   elAttr' "button" ("class" =: "btn btn-primary mx-1") $
      --     text "C-Continuations"

      -- let clicks1 = const (Just Glu) <$> domEvent Click e1
      let clicks2 = const (Just Ant) <$> domEvent Click e2
      -- let clicks3 = const (Just Fol) <$> domEvent Click e2
      -- let clicks4 = const (Just Brzo) <$> domEvent Click e4
      -- let clicks5 = const (Just Cont) <$> domEvent Click e5

      holdDyn Nothing $ leftmost [clicks2] -- old , clicks1, clicks3, clicks4, clicks5]

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

#ifdef ghcjs_HOST_OS

foreign import javascript unsafe "var im = Viz( $1 , { format: \"svg\" }); console.log(im); $r = im;"
  vizSVG :: JSString -> JSString

svgAut ::
  ( MonadWidget t m,
    ToString symbol,
    ToString state,
    Hashable symbol,
    Hashable state,
    Eq symbol,
    Eq state
  ) =>
  NFA state symbol ->
  m ()
svgAut auto = do
  _ <-
    el "figure" $
      elDynHtmlAttr' "td" ("class" =: "text-left pr-3") $
        constDyn $
          Te.pack $
            JS.unpack $
              vizSVG $
                JS.pack $
                  faToDot auto
  return ()

#else

svgAut ::
  ( MonadWidget t m
  ) =>
  NFA state symbol ->
  m ()
svgAut _ = pure ()

#endif

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
    Ord state
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
  _ <- elDynHtmlAttr' "td" ("class" =: "text-right pl-3") $ constDyn x
  _ <- elDynHtmlAttr' "td" ("class" =: "text-left pr-3") $ constDyn fols
  return ()
