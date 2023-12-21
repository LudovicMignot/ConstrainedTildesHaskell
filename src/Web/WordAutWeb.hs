{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.WordAutWeb where

import Control.Applicative ((<$>))
import Data.Foldable
import Data.Hashable
import Data.List as L
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text as Te
  ( Text,
    pack,
  )
import Exp
import NFA
import Reflex
import Reflex.Dom.Core
import ToString
import Web.Widget

-- import WordAuto.Conversion.Conversion
-- import WordAuto.ExpRat.ExpRat as E
-- import WordAuto.FA.DFA
-- import WordAuto.FA.FAClass

-- main :: IO ()
main = mainWidgetWithHead header body

header :: (MonadWidget t m) => m ()
header =
  elAttr
    "link"
    (Map.fromList [("rel", "stylesheet"), ("href", bootstrapCSSCDN)])
    $ return ()
  where
    bootstrapCSSCDN =
      "https://stackpath.bootstrapcdn.com/bootstrap/4.1.2/css/bootstrap.min.css"

body :: (MonadWidget t m) => m ()
body = do
  _ <- elAttr "div" ("class" =: "container") $ do
    elAttr "h1" ("class" =: "text-center") $ text "Word Automata Constructions"
    el "hr" $ return ()
    dynExp <- lecteurExp
    constructionDyn <- grpBout
    el "hr" $ return ()
    info <-
      holdDyn Nothing $
        transform
          <$> ffilter
            (\(exp_, _) -> isJust exp_)
            (attachPromptlyDyn dynExp (updated constructionDyn))
    dyn $ theContent <$> info
  footer
  where
    transform (Nothing, _) = Nothing
    transform (_, Nothing) = Nothing
    transform (Just a, Just b) = Just (a, b)

footer :: (MonadWidget t m) => m ()
footer = do
  elAttr "script" (Map.fromList [("defer", "defer"), ("src", jqueryCDN)]) $
    return ()
  elAttr "script" (Map.fromList [("defer", "defer"), ("src", popperCDN)]) $
    return ()
  elAttr "script" (Map.fromList [("defer", "defer"), ("src", bootstrapJsCDN)]) $
    return ()
  where
    jqueryCDN = "https://code.jquery.com/jquery-3.3.1.min.js"
    popperCDN =
      "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js"
    bootstrapJsCDN =
      "https://stackpath.bootstrapcdn.com/bootstrap/4.1.2/js/bootstrap.bundle.js"

theContent :: (MonadWidget t m) => Maybe (Exp Char, Method) -> m ()
theContent Nothing = blank
-------------------------------
-- theContent (Just (e, Glu)) = aux Glu e
-------------------------------
-- theContent (Just (e, Fol)) = aux Fol e
-------------------------------
theContent (Just (e, Ant)) = aux2 Ant e

-----------------------------------
-- theContent (Just (e, Brzo)) = aux2 Brzo e
-----------------------------------------
-- theContent (Just (e, Cont)) =
--   elAttr
--     "div"
--     ( Map.fromList
--         [ ( "style",
--             "display : flex; flex-direction : column; flex-wrap : wrap; justify-content : center; align-items : center; align-content : center;"
--           )
--         ]
--     )
--     $ do
--       let aut = ccontAutLin e
--       el "h4" $ text "C-continuations construction"
--       _ <-
--         el "p" $
--           el "h5" $
--             elDynHtml' "span" $
--               constDyn $
--                 mappend "E<sup>#</sup> = " $
--                   Te.pack $
--                     toHtmlString $
--                       linearize e

--       listTab
--         [ ("ccont", "C-Continuation computation"),
--           ("ccontAut", "C-Continuation automaton"),
--           ("termAut", "Derived terms automaton via c-continuations")
--         ]

--       elAttr "div" ("class" =: "tab-content") $ do
--         oneTab "ccont" True $
--           el "table" $
--             tableDraw $
--               L.map myFunc $
--                 stateList aut
--         oneTab "ccontAut" False $ svgAutWithMap aut
--         oneTab "termAut" False $ svgAutWithMap $ antimirovViaCCont e
--       return ()
--   where
--     myFunc (CStateInit f) = ("c<sub>ε</sub>(E) = ", Te.pack $ toHtmlString f)
--     myFunc (CState p f) =
--       ( mconcat ["c<sub>", Te.pack $ toHtmlString p, "</sub>(E) = "],
--         Te.pack $ toHtmlString f
--       )

aux2 :: (MonadWidget t m) => Method -> Exp Char -> m ()
aux2 meth e =
  elAttr
    "div"
    ( Map.fromList
        [ ( "style",
            "display : flex; flex-direction : column; flex-wrap : wrap; justify-content : center; align-items : center; align-content : center;"
          )
        ]
    )
    $ do
      el "h4" $ text title
      _ <-
        el "p" $
          el "h5" $
            elDynHtml' "span" $
              constDyn $
                mappend "E = " $
                  Te.pack $
                    toHtmlString e

      listTab [("id1", st1), ("id2", st2)]

      elAttr "div" ("class" =: "tab-content") $ do
        oneTab "id1" True $ el "table" $ tableDraw derivs
        oneTab "id2" False dotFA

      return ()
  where
    (st1, st2) = case meth of
      Ant -> ("Derived Terms", "Derived terms automaton")
      -- Brzo -> ("Derivatives", "Derivatives automaton")
      _ -> undefined
    (derivs, dotFA) = case meth of
      Ant ->
        let aut = antimirov e
         in ( L.map
                ( \((x, a), p) ->
                    ( mconcat
                        [ symbolDeriv,
                          "<sub>",
                          Te.pack $ toHtmlString a,
                          "</sub>(",
                          Te.pack $ toHtmlString x,
                          ") = "
                        ],
                      Te.pack $ toHtmlString p
                    )
                )
                $ Map.toList $
                  Map.fromListWith Set.union $
                    L.map (\(x, a, y) -> ((x, a), Set.singleton y)) $
                      transitionList aut,
              svgAutWithMap aut
            )
      -- Brzo ->
      --   let aut = brzozowskiDerAut e
      --    in ( L.map
      --           ( \(x, a, p) ->
      --               ( mconcat
      --                   [ symbolDeriv,
      --                     "<sub>",
      --                     Te.pack $ toHtmlString a,
      --                     "</sub>(",
      --                     Te.pack $ toHtmlString x,
      --                     ") = "
      --                   ],
      --                 Te.pack $ toHtmlString p
      --               )
      --           )
      --           $ transitionList aut,
      --         svgAutWithMap aut
      --       )
      _ -> undefined
    (title, symbolDeriv) = case meth of
      Ant -> ("Antimirov construction", "δ" :: Text)
      -- Brzo -> ("Brzozowski construction", "D")
      _ -> undefined

-- aux :: (MonadWidget t m) => Method -> Exp Char -> m ()
-- aux meth e =
--   elAttr
--     "div"
--     ( Map.fromList
--         [ ( "style",
--             "display : flex; flex-direction : column; flex-wrap : wrap; justify-content : center; align-items : center; align-content : center;"
--           )
--         ]
--     )
--     $ do
--       el "h4" $ text title
--       _ <-
--         el "p" $
--           el "h5" $
--             elDynHtml' "span" $
--               constDyn $
--                 mappend "E<sup>#</sup> = " $
--                   Te.pack $
--                     toHtmlString elin
--       _ <-
--         el "p" $
--           el "h6" $
--             elDynHtml' "span" $
--               constDyn $
--                 mappend "Pos(E<sup>#</sup>) = " $
--                   Te.pack $
--                     toHtmlString $
--                       pos elin

--       listTab
--         [ ("id1", st1),
--           ("id2", st2),
--           ("id3", st3),
--           ("id4", st4),
--           ("id5", st5),
--           ("id6", st6)
--         ]

--       elAttr "div" ("class" =: "tab-content") $ do
--         oneTab "id1" True $
--           el "table" $ do
--             _ <- el "tr" $ do
--               elAttr "td" ("class" =: "text-right pl-3") $ text "Null(E) = "
--               _ <-
--                 elDynHtmlAttr' "td" ("class" =: "text-left pr-3") $
--                   constDyn $
--                     Te.pack $
--                       toHtmlString $
--                         Exp.nullable e
--               elAttr "td" ("class" =: "text-right pl-3") $ text "First(E) = "
--               _ <-
--                 elDynHtmlAttr' "td" ("class" =: "text-left pr-3") $
--                   constDyn $
--                     Te.pack $
--                       toHtmlString $
--                         first elin
--               return ()
--             tableDraw $
--               ("Last(E) = ", Te.pack $ toHtmlString $ E.last elin) :
--               fol
--         oneTab "id2" False bloc1
--         oneTab "id3" False bloc2
--         oneTab "id4" False bloc3
--         oneTab "id5" False bloc4
--         oneTab "id6" False bloc5

--       return ()
--   where
--     (st1, st2, st3, st4, st5, st6) = case meth of
--       Glu ->
--         ( "Position functions table",
--           "Position automaton",
--           "Delinearized position automaton",
--           "Determinized",
--           "Renamed",
--           "Minimized"
--         )
--       Fol ->
--         ( "Position functions table",
--           "States as similar positions",
--           "States as follow sets",
--           "Delinearized follow automaton",
--           "",
--           ""
--         )
--       _ -> undefined
--     (title, bloc1, bloc2, bloc3, bloc4, bloc5) = case meth of
--       Glu ->
--         let g = glushkov e
--          in let g' = determinise g
--              in let g'' = (renameStates g' :: DFA Word Char)
--                  in ( "Glushkov construction",
--                       svgAut $ glushkovLin e,
--                       svgAut g,
--                       svgAut g',
--                       svgAut g'',
--                       svgAut $ trim $ mergeEqHopcroft $ g''
--                     )
--       Fol ->
--         ( "Follow construction",
--           svgAut $ followAutViaQuot e,
--           svgAut $ followAutViaFun e,
--           svgAut $ followAut e,
--           blank,
--           blank
--         )
--       _ -> undefined
--     elin = linearize e
--     fol =
--       L.map
--         ( \(x, fols) ->
--             ( mconcat ["Follow(E,", Te.pack $ toHtmlString x, ") = "],
--               Te.pack $ toHtmlString fols
--             )
--         )
--         $ sortBy (\(Pos _ n1, _) (Pos _ n2, _) -> compare n1 n2) $
--           foldMap (\x -> [(x, follow x elin)]) $
--             pos elin

listTab :: MonadWidget t m => [(Text, Text)] -> m ()
listTab [] = blank
listTab l =
  elAttr "ul" (Map.fromList [("class", "nav nav-tabs"), ("role", "tablist")]) $
    listeTab' $
      L.reverse l
  where
    listeTab' [] = blank
    listeTab' [(t1, t2)] =
      elAttr "li" ("class" =: "nav-item") $
        elAttr
          "a"
          ( Map.fromList
              [ ("class", "nav-link active"),
                ("id", mappend t1 "-tab"),
                ("data-toggle", "tab"),
                ("href", mappend "#" t1),
                ("role", "tab"),
                ("aria-controls", t1),
                ("aria-selected", "false")
              ]
          )
          $ text t2
    listeTab' ((t1, t2) : reste) = do
      listeTab' reste
      elAttr "li" ("class" =: "nav-item") $
        elAttr
          "a"
          ( Map.fromList
              [ ("class", "nav-link"),
                ("id", mappend t1 "-tab"),
                ("data-toggle", "tab"),
                ("href", mappend "#" t1),
                ("role", "tab"),
                ("aria-controls", t1),
                ("aria-selected", "false")
              ]
          )
          $ text t2

oneTab :: MonadWidget t m => Text -> Bool -> m () -> m ()
oneTab ident isActive bloc =
  elAttr
    "div"
    ( Map.fromList
        [ ( "class",
            mappend "mt-5 flex-column align-items-center tab-pane fade" active
          ),
          ("id", ident),
          ("role", "tabpanel"),
          ("aria-labelledby", mappend ident "-tab")
        ]
    )
    $ do
      _ <-
        elAttr
          "div"
          ("class" =: "d-flex flex-column align-items-center")
          bloc
      blank
  where
    active
      | isActive = " show active"
      | otherwise = ""
