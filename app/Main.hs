module Main (main) where

import Language.Javascript.JSaddle.Warp (run)
import WordAutWeb (mainFun)

main :: IO ()
main = run 3911 mainFun
