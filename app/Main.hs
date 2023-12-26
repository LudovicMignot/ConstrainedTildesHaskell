{-# LANGUAGE CPP #-}

module Main (main) where

import Language.Javascript.JSaddle.Warp
import WordAutWeb

#ifdef ghcjs_HOST_OS
main :: IO ()
main = mainFun

#else

main :: IO ()
main =
#ifndef ghcjs_HOST_OS
  run 3911 
#endif
    mainFun

#endif