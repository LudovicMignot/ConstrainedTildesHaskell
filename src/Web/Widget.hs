{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Widget where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as Set
import Data.Hashable
import Data.JSString as JS
import qualified Data.Map as Map
import Data.Maybe
import Data.Text as Te
  ( Text,
    pack,
    unpack,
  )
import Reflex
import Reflex.Dom
import ToString
import WordAuto.ExpRat.ExpRat as E
import WordAuto.ExpRat.ExpRatFromString
import WordAuto.FA.FAClass

foreign import javascript unsafe "var im = Viz( $1 , { format: \"svg\" }); console.log(im); $r = im;"
  vizSVG :: JSString -> JSString

diceButton :: (MonadWidget t m) => m (Event t ())
diceButton = do
  (e, _) <- elAttr' "div" ("class" =: "btn btn-primary mx-1") $ text "⚃"
  return $ () <$ domEvent Click e

data Method = Glu | Fol | Ant | Brzo | Cont

lecteurExp :: (MonadWidget t m) => m (Dynamic t (Maybe (ExpRat Char)))
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

          let eDef = fromString "(a+b)*.a.(a+b)"

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
      (e1, _) <-
        elAttr' "button" ("class" =: "btn btn-primary mx-1") $
          text "Glushkov"
      (e2, _) <-
        elAttr' "button" ("class" =: "btn btn-primary mx-1") $
          text "Follow"
      (e3, _) <-
        elAttr' "button" ("class" =: "btn btn-primary mx-1") $
          text "Antimirov"
      (e4, _) <-
        elAttr' "button" ("class" =: "btn btn-primary mx-1") $
          text "Brzozowski"
      (e5, _) <-
        elAttr' "button" ("class" =: "btn btn-primary mx-1") $
          text "C-Continuations"

      let clicks1 = const (Just Glu) <$> domEvent Click e1
      let clicks2 = const (Just Fol) <$> domEvent Click e2
      let clicks3 = const (Just Ant) <$> domEvent Click e3
      let clicks4 = const (Just Brzo) <$> domEvent Click e4
      let clicks5 = const (Just Cont) <$> domEvent Click e5

      holdDyn Nothing $ leftmost [clicks1, clicks2, clicks3, clicks4, clicks5]

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
              $ elDynHtml' "span" $
                constDyn "&times"
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
  m (Dynamic t (Maybe (ExpRat Char)))
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

      let result = (fromString . Te.unpack) <$> _textInput_value n
          attrs = fmap (maybe errorState (const validState)) result
  return result

-- expInput :: (MonadWidget t m) => Text -> m (Dynamic t (Maybe (ExpRat Char)))
-- expInput ident = do
--   let errorState =
--         Map.fromList [("class", "form-control is-invalid"), ("id", ident)]
--       validState =
--         Map.fromList [("class", "form-control is-valid"), ("id", ident)]
--   rec n <-
--         textInput
--         $  def
--         &  textInputConfig_inputType
--         .~ "text"
--         &  textInputConfig_initialValue
--         .~ "a.a.b*.a+b.b.a*.b+a.b.a.b.(a+b)*.a"
--         &  textInputConfig_attributes
--         .~ attrs
--       let result = (fromString . Te.unpack) <$> _textInput_value n
--           attrs  = fmap (maybe errorState (const validState)) result
--   return result

svgAut ::
  ( MonadWidget t m,
    FA fa,
    ToString symbol,
    ToString state,
    Hashable symbol,
    Hashable state,
    Eq symbol,
    Eq state
  ) =>
  fa state symbol ->
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

svgAutWithMap ::
  ( MonadWidget t m,
    FA fa,
    ToString symbol,
    ToString state,
    Hashable symbol,
    Hashable state,
    Eq symbol,
    Eq state
  ) =>
  fa state symbol ->
  m ()
svgAutWithMap auto = do
  let thestates = Prelude.zip (stateList auto) [1 :: Word ..]
  let themap = HMap.fromList thestates
  let auto' = renameViaMap auto themap
  svgAut auto'
  let info =
        Prelude.map
          (\(s, i) -> (Te.pack $ show i ++ " = ", Te.pack $ toHtmlString s))
          $ HMap.toList themap
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
