{-# LANGUAGE ImportQualifiedPost #-}

module ExpFromString where

import Control.Monad (join)
import Exp (Exp)
import ExpAlex (alexScanTokens)
import ExpHappy qualified as H
  ( expfromString,
  )

expFromString ::
  String -> Maybe (Exp Char)
expFromString = join . H.expfromString . alexScanTokens
