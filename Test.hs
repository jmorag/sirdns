{-# LANGUAGE TemplateHaskell #-}
import RIO
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)

import Types
import Parser

main = defaultMain $
  testGroup "Tests" [testProperty "parse and serialize are inverses" prop_roundtrip]

genHeader :: Gen Header
genHeader = undefined

genName :: Gen Name
genName = undefined

genQuestion :: Gen Question
genQuestion = undefined

genRecord :: Gen Record
genRecord = undefined

genQuery :: Gen Query
genQuery = undefined

prop_roundtrip :: Property
prop_roundtrip = property $ do
  query <- forAll genQuery
  queryP (queryToByteString query) === query

-- To run in ghci
tests :: IO Bool
tests = checkParallel $$(discover)
