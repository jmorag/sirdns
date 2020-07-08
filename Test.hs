{-# LANGUAGE TemplateHaskell #-}

import Data.Bit
import Data.IP
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Parser
import RIO
import RIO.Text (unpack)
import qualified RIO.ByteString as B
import Types

genHeader :: Gen Header
genHeader = do
  _id <- Gen.word16 Range.linearBounded
  _qr <- fmap Bit Gen.bool
  _opcode <- Gen.word8 (Range.linearFrom 0 0 15)
  _aa <- fmap Bit Gen.bool
  _tc <- fmap Bit Gen.bool
  _rd <- fmap Bit Gen.bool
  _ra <- fmap Bit Gen.bool
  _z <- pure 0
  _rcode <- Gen.word8 (Range.linearFrom 0 0 15)
  _qdcount <- Gen.word16 Range.linearBounded
  _ancount <- Gen.word16 Range.linearBounded
  _nscount <- Gen.word16 Range.linearBounded
  _arcount <- Gen.word16 Range.linearBounded
  pure $ Header {..}

genName :: Gen Name
genName = Gen.bytes (Range.linearFrom 1 1 63) & Gen.list (Range.linear 0 10) <&> Name

genQuestion :: Gen Question
genQuestion = do
  _qname <- genName
  _qtype <- Gen.element [A, CNAME, NAMESERVER, AAAA]
  pure $ Question {..}

genRecord :: Gen Record
genRecord = do
  _name <- genName
  _ttl <- Gen.word32 Range.linearBounded
  _rdata <-
    Gen.choice
      [ ARecord <$> Gen.enumBounded,
        CName <$> genName,
        NameServer <$> genName,
        AAAARecord <$> Gen.enumBounded
      ]
  let _rdlength = case _rdata of
        ARecord _ -> 4
        CName n -> foldl' (\acc b -> acc + fromIntegral (B.length b + 1)) 1 (n ^. labels)
        NameServer n -> foldl' (\acc b -> acc + fromIntegral (B.length b + 1)) 1 (n ^. labels)
        AAAARecord _ -> 16
  pure $ Record {..}

genQuery :: Gen Query
genQuery = do
  _header <- genHeader
  _question <- Gen.list (singleton' (_header ^. qdcount)) genQuestion
  _answer <- Gen.list (singleton' (_header ^. ancount)) genRecord
  _authority <- Gen.list (singleton' (_header ^. nscount)) genRecord
  _additional <- Gen.list (singleton' (_header ^. arcount)) genRecord
  pure $ Query {..}
  where
    singleton' = Range.singleton . fromIntegral


prop_roundtrip :: Property
prop_roundtrip = property $ do
  query <- forAll genQuery
  tripping query queryToByteString parseQuery

-- To run in ghci
tests :: IO Bool
tests = checkParallel $$(discover)

main = do
  t <- tests
  if t then exitSuccess else exitFailure
