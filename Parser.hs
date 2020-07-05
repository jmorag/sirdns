module Parser where

import Control.Lens (_1)
import Data.Bit
import Data.Bits
import Data.IP
import Text.Megaparsec
import RIO hiding (id)
import qualified RIO.ByteString as B
import Types

type Parser a = ByteString -> Int -> (a, Int)

word16P :: Parser Word16
word16P bytes offset =
  let b0 = fromIntegral $ B.index bytes offset
      b1 = fromIntegral $ B.index bytes (offset + 1)
   in (shiftL b0 8 + b1, offset + 2)

qrP :: Parser Bit
qrP bytes offset = (Bit $ testBit (B.index bytes offset) 7, offset)

opCodeP :: Parser Word8
opCodeP bytes offset = (shiftR (B.index bytes offset) 3 `mod` 16, offset)

aaP :: Parser Bit
aaP bytes offset = (Bit $ testBit (B.index bytes offset) 2, offset)

tcP :: Parser Bit
tcP bytes offset = (Bit $ testBit (B.index bytes offset) 1, offset)

rdP :: Parser Bit
rdP bytes offset = (Bit $ testBit (B.index bytes offset) 0, offset + 1)

raP :: Parser Bit
raP bytes offset = (Bit $ testBit (B.index bytes offset) 7, offset)

zP :: Parser Word8
zP bytes offset = (shiftR (B.index bytes offset) 4 `mod` 8, offset)

rCodeP :: Parser Word8
rCodeP bytes offset = ((B.index bytes offset) `mod` 16, offset + 1)

nameP :: Parser Name
nameP bytes offset = let (name, offset', ptrOffset) = go offset Nothing
                      in case ptrOffset of
                           Nothing -> (Name name, offset')
                           Just o -> (Name name, o)
  where
    go :: Int -> Maybe Int -> ([ByteString], Int, Maybe Int)
    go !offset !ptrOffset =
        let len' = B.index bytes offset 
            len = fromIntegral len' in
        if | len == 0 -> ([], offset + 1, ptrOffset)
           | shiftR len' 6 == 3 ->
             let offset' = shiftL (len' `mod` (2^5)) 8 + B.index bytes (offset + 1)
             in go (fromIntegral offset') (case ptrOffset of Nothing -> Just (offset + 2); Just o -> Just o)
           | otherwise -> let (next, offset', ptrOffset') = go (offset + 1 + len) ptrOffset in
             ((B.take len (B.drop (offset + 1) bytes)) : next, offset', ptrOffset')

word32P :: Parser Word32
word32P bytes offset = 
  let b0 = shiftR (B.index bytes offset) 24
      b1 = shiftR (B.index bytes (offset + 1)) 16
      b2 = shiftR (B.index bytes (offset + 2)) 8
      b3 = B.index bytes (offset + 3)
   in (sum (map fromIntegral [b0,b1,b2,b3]), offset + 4)

headerP :: Parser Header
headerP bytes offset =
  let (_id, o1) = word16P bytes offset
      (_qr, o2) = qrP bytes o1
      (_opcode, o3) = opCodeP bytes o2
      (_aa, o4) = aaP bytes o3
      (_tc, o5) = tcP bytes o4
      (_rd, o6) = rdP bytes o5
      (_ra, o7) = raP bytes o6
      (_z, o8) = zP bytes o7
      (_rcode, o9) = rCodeP bytes o8
      (_qdcount, o10) = word16P bytes o9
      (_ancount, o11) = word16P bytes o10
      (_nscount, o12) = word16P bytes o11
      (_arcount, o13) = word16P bytes o12
   in (Header {..}, o13)

questionP :: Parser Question
questionP bytes offset =
  let (_qname, o1) = nameP bytes offset
      (_qtype, o2) = word16P bytes o1
      (_qclass, o3) = word16P bytes o2
      _qtype' = case _qtype of 0x1 -> A;
                               0x2 -> NAMESERVER;
                               0x5 -> CNAME;
                               other -> error $ show other <> " this QTYPE is not supported"
   in case _qclass of 0x1 -> (Question _qname _qtype', o3)
                      other -> error $ show other <> " not supported"

recordP :: Parser Record
recordP bytes offset =
  let (_name, o1) = nameP bytes offset
      (_type, o2) = word16P bytes o1
      (_class, o3) = word16P bytes o2
      (_ttl, o4) = word32P bytes o3
      (_rdlength, o5) = word16P bytes o4
      (_rdata, o6) = case _type of
                       0x1 -> over _1 (ARecord . toIPv4w) (word32P bytes o5)
                       0x2 -> over _1 NameServer (nameP bytes o5)
                       0x5 -> over _1 CName (nameP bytes o5)
                       other -> error $ show other <> " this RDATA type is not supported"
  in (Record _name _ttl _rdata, o6)
