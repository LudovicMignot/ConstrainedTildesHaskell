module ToString where

import Data.Char (toUpper)
import Data.Finite
import Data.List (intercalate)
import Data.Set (Set)
import Data.Set qualified as Set

class ToString a where
  toHtmlString :: a -> String
  toHtmlString = toString

  -- pour dot, passer les balises en capitales
  -- obligatoire ?
  toHtmlCapString :: a -> String
  toHtmlCapString x = change $ toHtmlString x
    where
      change "" = ""
      change ('<' : l) = '<' : change' l
      change (y : l) = y : change l
      change' "" = ""
      change' ('>' : l) = '>' : change l
      change' (y : l) = toUpper y : change' l

  toString :: a -> String

  myPrintLn :: a -> IO ()
  myPrintLn x = putStrLn $ toString x

instance ToString Bool where
  toString True = "true"
  toString False = "false"

instance ToString () where
  toString _ = "\"( )\""

instance ToString Char where
  toString a = [a]

instance ToString Int where
  toString = show

instance ToString Word where
  toString = show

instance ToString a => ToString (Set a) where
  toString s
    | Set.null s = "∅"
    | otherwise = "{" ++ intercalate "," (map toString $ Set.toList s) ++ "}"

  toHtmlString s
    | Set.null s = "∅"
    | otherwise = "{" ++ intercalate "," (map toHtmlString $ Set.toList s) ++ "}"

instance (ToString a, ToString b) => ToString (a, b) where
  toString (x, y) = "(" ++ toString x ++ "," ++ toString y ++ ")"

  toHtmlString (x, y) = "(" ++ toHtmlString x ++ "," ++ toHtmlString y ++ ")"

instance (ToString a, ToString b, ToString c) => ToString (a, b, c) where
  toString (x, y, z) = "(" ++ toString x ++ "," ++ toString y ++ "," ++ toString z ++ ")"

  toHtmlString (x, y, z) = "(" ++ toHtmlString x ++ "," ++ toHtmlString y ++ "," ++ toHtmlString z ++ ")"

instance ToString a => ToString [a] where
  toString l = intercalate "," $ map toString l

  toHtmlString l = intercalate "," $ map toHtmlString l

instance ToString a => ToString (Maybe a) where
  toString Nothing = "Nothing"
  toString (Just x) = "Just" ++ toString x

instance ToString (Finite n) where
  toString = show . getFinite