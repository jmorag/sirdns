module Types where

import Control.Lens
import Data.Bits
import Data.ByteString.Internal
import Data.ByteString.Char8 (pack)
import Data.String
import RIO hiding
  ( ASetter,
    ASetter',
    Getting,
    Lens,
    Lens',
    SimpleGetter,
    (^.),
    id,
    lens,
    over,
    set,
    sets,
    to,
    view,
  )
import qualified RIO.ByteString as B

newtype DNSQuery = DNSQuery {_bytes :: ByteString}
  deriving (Show)

bytes :: Lens' DNSQuery ByteString
bytes = lens _bytes (\x y -> x {_bytes = y})

type Offset = DNSQuery -> Int -> Int

calculateOffset :: DNSQuery -> [Offset] -> Int
calculateOffset dns = foldl' (\acc offset -> acc + offset dns acc) 0

word16Lens :: [Offset] -> Lens' DNSQuery Word16
word16Lens offsets = lens getter setter
  where
    getter dns = getWord16 (calculateOffset dns offsets) dns
    setter dns = setWord16 (calculateOffset dns offsets) dns

getWord16 :: Int -> DNSQuery -> Word16
getWord16 offset dns =
  let b1 = fromIntegral $ dns ^?! bytes . ix offset
      b2 = fromIntegral $ dns ^?! bytes . ix (offset + 1)
   in shiftL b1 8 + b2

setWord16 :: Int -> DNSQuery -> Word16 -> DNSQuery
setWord16 offset dns word =
  let b1 = fromIntegral $ shiftR word 8
      b2 = fromIntegral word
   in dns & bytes . ix offset .~ b1
          & bytes . ix (offset + 1) .~ b2

word32Lens :: [Offset] -> Lens' DNSQuery Word32
word32Lens offsets = lens getter setter
  where
    getter dns = getWord32 (calculateOffset dns offsets) dns
    setter dns = setWord32 (calculateOffset dns offsets) dns

getWord32 :: Int -> DNSQuery -> Word32
getWord32 offset dns =
  let b1 = shiftL (dns ^?! bytes . ix offset) 24
      b2 = shiftL (dns ^?! bytes . ix (offset + 1)) 16
      b3 = shiftL (dns ^?! bytes . ix (offset + 2)) 8
      b4 = dns ^?! bytes . ix (offset + 3)
   in sum (map fromIntegral [b1, b2, b3, b4])

setWord32 :: Int -> DNSQuery -> Word32 -> DNSQuery
setWord32 offset dns word =
  let b1 = fromIntegral $ shiftR word 24
      b2 = fromIntegral $ shiftR word 16
      b3 = fromIntegral $ shiftR word 8
      b4 = fromIntegral word
   in dns & bytes . ix offset .~ b1
          & bytes . ix (offset + 1) .~ b2
          & bytes . ix (offset + 2) .~ b3
          & bytes . ix (offset + 3) .~ b4

constantOffset :: Int -> Offset
constantOffset i _query currentOffset = i + currentOffset

id :: Lens' DNSQuery Word16
id = word16Lens []

qdCount :: Lens' DNSQuery Word16
qdCount = word16Lens [constantOffset 4]

anCount :: Lens' DNSQuery Word16
anCount = word16Lens [constantOffset 6]

nsCount :: Lens' DNSQuery Word16
nsCount = word16Lens [constantOffset 8]

arCount :: Lens' DNSQuery Word16
arCount = word16Lens [constantOffset 10]

bitLens :: Int -> Int -> Lens' DNSQuery Bool
bitLens byte bitIdx = lens getter setter
  where
    getter dnsHeader = (dnsHeader ^?! bytes . ix byte) & \byte' -> testBit byte' bitIdx
    setter dnsHeader b =
      over
        (bytes . ix byte)
        (\byte' -> if b then setBit byte' bitIdx else clearBit byte' bitIdx)
        dnsHeader

qr :: Lens' DNSQuery Bool
qr = bitLens 2 7

aa :: Lens' DNSQuery Bool
aa = bitLens 2 2

tc :: Lens' DNSQuery Bool
tc = bitLens 2 1

rd :: Lens' DNSQuery Bool
rd = bitLens 2 0

ra :: Lens' DNSQuery Bool
ra = bitLens 3 7

opCode :: Lens' DNSQuery Word8
opCode = lens getter setter
  where
    getter dnsHeader = (dnsHeader ^?! bytes . ix 2)
      & \byte -> shiftR byte 3 `mod` (2 ^ 4)
    setter dnsHeader code =
      let setOpCode byte = setBitSlice byte code 4 3
       in over (bytes . ix 2) setOpCode dnsHeader

rCode :: Lens' DNSQuery Word8
rCode = lens getter setter
  where
    getter dnsHeader = (dnsHeader ^?! bytes . ix 3) & \byte -> byte `mod` (2 ^ 4)
    setter dnsHeader code =
      let setRCode byte = setBitSlice byte code 4 0
       in over (bytes . ix 2) setRCode dnsHeader

z :: Lens' DNSQuery Word8
z = lens getter setter
  where
    getter dnsHeader = (dnsHeader ^?! bytes . ix 3)
      & \byte -> shiftR byte 4 `mod` (2 ^ 3)
    setter dnsHeader code =
      let setRCode byte = setBitSlice byte code 3 4
       in over (bytes . ix 2) setRCode dnsHeader

setBitSlice :: Word8 -> Word8 -> Int -> Int -> Word8
setBitSlice word bits len offset =
  foldr (\bitIdx acc -> assignBit (testBit bits bitIdx) (bitIdx + offset) acc) word
    [0..len - 1]

assignBit :: Bits a => Bool -> Int -> a -> a
assignBit True idx word = setBit word idx
assignBit False idx word = clearBit word idx

newtype Name = Name { unName :: [ByteString] }
  deriving (Eq)
instance Show Name where
  show (Name strs) = show $ B.intercalate "." strs
instance IsString Name where
  fromString s = Name (B.split (c2w '.') (pack s))

nameLens :: [DNSQuery -> Int -> Int] -> Lens' DNSQuery Name
nameLens offsets = lens getter setter
  where
    getter dns = getName (calculateOffset dns offsets) dns
    -- does not handle compression
    setter dns = setName (calculateOffset dns offsets) dns

getName :: Int -> DNSQuery -> Name
getName off dns = Name (go off)
  where
    go offset =
      let len' = dns ^?! bytes . ix offset
          len = fromIntegral len'
       in if  | shiftR len' 6 == 3 ->
                go . fromIntegral $
                  shiftL (len' `mod` 2 ^ 5) 8 + (dns ^?! bytes . ix (offset + 1))
              | len == 0 -> []
              | otherwise ->
                B.pack (map (B.index (dns ^. bytes)) [offset + 1 .. offset + len])
                  : go (offset + len + 1)

setName :: Int -> DNSQuery -> Name -> DNSQuery
setName offset dns (Name q) =
  let (header, rest) = B.splitAt offset (dns ^. bytes)
      qnameBytes = foldr appendPart (B.singleton 0) q
      appendPart part acc = B.cons (fromIntegral $ B.length part) part <> acc
      totalLen = B.length qnameBytes
      rest' = B.drop totalLen rest
   in DNSQuery $ header <> qnameBytes <> rest'

nameLen :: DNSQuery -> Int -> Int
nameLen dns offset =
  let thisByte = dns ^?! bytes . ix offset
  in if | thisByte == 0 -> 1
        | shiftR thisByte 6 == 3 -> 2
        | otherwise -> let b = fromIntegral thisByte
                        in 1 + b + nameLen dns (offset + b)

qName :: Lens' DNSQuery Name
qName = nameLens [constantOffset 12]

qType :: Lens' DNSQuery Word16
qType = word16Lens [constantOffset 12, nameLen]

qClass :: Lens' DNSQuery Word16
qClass = word16Lens [constantOffset 12, nameLen, constantOffset 2]

name :: Lens' DNSQuery Name
name = nameLens [constantOffset 12, nameLen, constantOffset 4]

type' :: Lens' DNSQuery Word16
type' = word16Lens [constantOffset 12, nameLen, constantOffset 4, nameLen]

class' :: Lens' DNSQuery Word16
class' =
  word16Lens
    [constantOffset 12, nameLen, constantOffset 4, nameLen, constantOffset 2]

ttl :: Lens' DNSQuery Word32
ttl =
  word32Lens
    [constantOffset 12, nameLen, constantOffset 4, nameLen, constantOffset 4]

rdLength :: Lens' DNSQuery Word16
rdLength =
  word16Lens
    [constantOffset 12, nameLen, constantOffset 4, nameLen, constantOffset 8]

data RData = IP Word32 | CName Name
  deriving (Show)

rData :: Lens' DNSQuery RData
rData = lens getter setter
  where
    offsetList =
      [constantOffset 12, nameLen, constantOffset 4, nameLen, constantOffset 10]
    getter dns =
      let offset = calculateOffset dns offsetList
       in case view type' dns of
            0x1 -> IP $ getWord32 offset dns
            0x5 -> CName $ getName offset dns
            0x2 -> error "Name server not supported"
            0xf -> error "Mail server not supported"
            other -> error $ "RDATA type " <> show other <> " not supported"
    setter dns rdata =
      let offset = calculateOffset dns offsetList
       in case rdata of
            IP ip -> setWord32 offset dns ip & set type' 0x1
            CName cname -> setName offset dns cname & set type' 0x5
