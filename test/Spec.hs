import ArbitraryExp
import Control.Monad
import Exp
import NFA
import Positions
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import ToString

genChar :: Gen Char
genChar = elements ['a' .. 'e']

genString :: Gen String
genString = resize 10 $ listOf genChar

prop_glu_anti :: Exp Char -> Property
prop_glu_anti e =
  let a = antimirov e
      g = glushkov e
   in forAll genString $ \w ->
        a `recognizes` w == g `recognizes` w

main :: IO ()
main = forM_ [1 .. 10] $ \_ -> do
  e <- getAlea 15
  putStrLn $ toString e
  hspec $ do
    describe "Antimirov automaton" $ do
      modifyMaxSuccess (const 1000000) $
        it "is equivalent to the Glushkov automaton" $
          property $ prop_glu_anti e

-- do
-- e <- getAlea 10
-- putStrLn $ toString e
-- quickCheck $ withMaxSuccess 10000 $ prop_glu_anti e
