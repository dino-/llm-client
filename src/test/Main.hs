-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite and has great output.
-- See its website for help:
-- <https://github.com/UnkindPartition/tasty>

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck


main :: IO ()
main = defaultMain $ testGroup " tests"
  [ unitTests
  , quickcheckProperties
  ]


unitTests :: TestTree
unitTests = testGroup "unit tests"
  [ testCase "True is True" $
      True @=? True
  ]


quickcheckProperties :: TestTree
quickcheckProperties = testGroup "quickcheck properties"
  [ testProperty "reverse twice results in the same list" $
      ((\l -> (reverse . reverse $ l) == l) :: [Char] -> Bool)
  ]
