{-# LANGUAGE CPP #-}

module Main (main) where

import WordAutWeb

#ifdef ghcjs_HOST_OS
main :: IO ()
main = mainFun

#else

main :: IO ()
main = return ()

#endif