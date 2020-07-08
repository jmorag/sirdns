module Parser where

import Control.Lens (_1)
import Data.Bit
import Data.Bits
import Data.ByteString.Builder
import Data.IP
import RIO hiding (id)
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import RIO.ByteString.Lazy (fromChunks)
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
rCodeP bytes offset = (B.index bytes offset `mod` 16, offset + 1)

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
             (B.take len (B.drop (offset + 1) bytes) : next, offset', ptrOffset')

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
                               28 -> AAAA
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
                       28 -> over _1 (AAAARecord . toIPv6 . map fromIntegral) (n word16P 8 bytes 05)
                       other -> error $ show other <> " this RDATA type is not supported"
  in (Record _name _ttl _rdlength _rdata, o6)


n :: Integral num => Parser a -> num -> Parser [a]
n _ 0 _ offset = ([], offset)
n parser num bytes offset = 
    let (this, offset') = parser bytes offset
        (next, offset'') = n parser (num - 1) bytes offset' in
      (this : next, offset'')

queryP :: ByteString -> Query
queryP bytes =
  let (_header, o1) = headerP bytes 0
      (_question, o2) = n questionP (_qdcount _header) bytes o1
      (_answer, o3) = n recordP (_ancount _header) bytes o2
      (_authority, o4) = n recordP (_nscount _header) bytes o3
      (_additional, _) = n recordP (_arcount _header) bytes o4
  in Query {..}

queryToByteString :: Query -> ByteString
queryToByteString q =
  toStrictBytes . toLazyByteString $
    mconcat [headerToBuilder (q ^. header),
             foldMap questionToBuilder (q ^. question),
             foldMap recordToBuilder (q ^. answer),
             foldMap recordToBuilder (q ^. authority),
             foldMap recordToBuilder (q ^. additional)]


headerToBuilder :: Header -> Builder
headerToBuilder h =
  mconcat
    [ word16BE (h ^. id),
      word8 $
        shiftL (h ^. opcode) 3 & assignBit (h ^. qr) 7
          & assignBit (h ^. aa) 2
          & assignBit (h ^. tc) 1
          & assignBit (h ^. rd) 0,
      word8 $
        shiftL (h ^. z) 4 + (h ^. rcode) & assignBit (h ^. ra) 7,
      word16BE (h ^. qdcount),
      word16BE (h ^. ancount),
      word16BE (h ^. nscount),
      word16BE (h ^. arcount)
    ]

nameToBuilder :: Name -> Builder
nameToBuilder (Name n) =
  foldMap (\b -> word8 (fromIntegral (B.length b)) <> byteString b) n <> word8 0

qTypeToBuilder :: TYPE -> Builder
qTypeToBuilder t =
  word16BE (case t of A -> 1
                      CNAME -> 5
                      NAMESERVER -> 2
                      AAAA -> 28)

questionToBuilder :: Question -> Builder
questionToBuilder q =
  nameToBuilder (q ^. qname)
  <> qTypeToBuilder (q ^. qtype)
  <> word16BE 1

rDataToBuilder :: RData -> Builder
rDataToBuilder r =
  case r of ARecord ip -> word32BE $ fromIPv4w ip
            CName n -> nameToBuilder n
            NameServer n -> nameToBuilder n
            AAAARecord ip -> mconcat $ map (word8 . fromIntegral) (fromIPv6b ip)

recordToBuilder :: Record -> Builder
recordToBuilder r =
  nameToBuilder (r ^. name)
  <> qTypeToBuilder case r ^. rdata of ARecord _ -> A
                                       CName _ -> CNAME
                                       NameServer _ -> NAMESERVER
                                       AAAARecord _ -> AAAA
  <> word16BE 1
  <> word32BE (r ^. ttl)
  <> word16BE (r ^. rdlength)
  <> rDataToBuilder (r ^. rdata)

assignBit :: Bits a => Bit -> Int -> a -> a
assignBit (Bit b) ix word = if b then setBit word ix else clearBit word ix
