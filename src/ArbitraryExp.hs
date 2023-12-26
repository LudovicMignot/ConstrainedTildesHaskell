{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module ArbitraryExp where

import BoolForm
import Data.Finite
import Data.Vector.Sized
import Exp
import Test.QuickCheck as Q

getAlea :: Int -> IO (Exp Char)
getAlea n = Q.generate $ resize n arbitrary

instance Arbitrary (Exp Char) where
  arbitrary = sized sizedExp

sizedExp :: Int -> Gen (Exp Char)
sizedExp 0 =
  frequency
    [ (8, Symbol <$> elements ['a' .. 'e']),
      (1, elements [Epsilon, Empty])
    ]
sizedExp n = do
  frequency
    [ ( 1,
        do
          phi <- sizedBoolForm 3
          k1 <- choose (0, n - 1)
          k2 <- choose (0, k1)
          e1 <- sizedExp (n - 1 - k1)
          e2 <- sizedExp (k1 - k2)
          e3 <- sizedExp k2
          return $ ConsTilde phi $ fromTuple (e1, e2, e3)
      ),
      ( 1,
        do
          e <- sizedExp (n - 1)
          return $ Star e
      ),
      ( 2,
        do
          k' <- choose (0, n - 1)
          op <- elements [conc, plus]
          e1 <- sizedExp (n - 1 - k')
          e2 <- sizedExp k'
          return $ op e1 e2
      )
    ]

sizedBoolForm :: Int -> Gen (BoolForm (Finite 3))
sizedBoolForm 0 =
  frequency
    [ (8, Atom <$> elements [0 :: Finite 3, 1, 2]),
      (1, elements [Bot, Top])
    ]
sizedBoolForm n = do
  frequency
    [ ( 1,
        do
          e <- sizedBoolForm (n - 1)
          return $ smartNeg e
      ),
      ( 2,
        do
          k' <- choose (0, n - 1)
          op <- elements [smartAnd, smartOr]
          e1 <- sizedBoolForm (n - 1 - k')
          e2 <- sizedBoolForm k'
          return $ op e1 e2
      )
    ]