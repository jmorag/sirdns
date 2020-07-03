module Main where

import Control.Lens
import Data.Bits
import Data.Char
import Data.String
import Network.Socket
import RIO hiding
  ( ASetter,
    ASetter',
    Getting,
    Lens,
    Lens',
    SimpleGetter,
    (^.),
    lens,
    over,
    set,
    sets,
    to,
    view,
  )
import qualified RIO.ByteString as B
import qualified RIO.Text as T
import qualified Prelude as P

main :: IO ()
main = do
  packet <- DNSQuery <$> B.readFile "dns-server-tests/test1/packet"
  P.print (packet ^. qdCount)

testPacket :: Int -> IO DNSQuery
testPacket i = DNSQuery <$> B.readFile ("dns-server-tests/test" <> show i <> "/packet")

sock :: IO Socket
sock = socket AF_INET Datagram defaultProtocol

newtype DNSQuery = DNSQuery {_bytes :: ByteString}

bytes :: Lens' DNSQuery ByteString
bytes = lens _bytes (\x y -> x {_bytes = y})

word16Lens :: [DNSQuery -> Int] -> Lens' DNSQuery Word16
word16Lens offsets = lens getter setter
  where
    getter dnsHeader =
      let byte = sum (map ($ dnsHeader) offsets)
          firstByte = fromIntegral $ B.index (dnsHeader ^. bytes) byte
          secondByte = fromIntegral $ B.index (dnsHeader ^. bytes) (byte + 1)
       in shiftL firstByte 8 + secondByte
    setter dnsHeader w =
      let byte = sum (map ($ dnsHeader) offsets)
          firstByte = shiftR w 8 & fromIntegral
          secondByte = fromIntegral w
       in set (bytes . ix byte) firstByte dnsHeader & set (bytes . ix (byte + 1)) secondByte


id :: Lens' DNSQuery Word16
id = word16Lens []

qdCount :: Lens' DNSQuery Word16
qdCount = word16Lens [const 4]

anCount :: Lens' DNSQuery Word16
anCount = word16Lens [const 6]

nsCount :: Lens' DNSQuery Word16
nsCount = word16Lens [const 8]

arCount :: Lens' DNSQuery Word16
arCount = word16Lens [const 10]

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
  fromString s = Name (B.split (fromIntegral (ord '.')) (encodeUtf8 (T.pack s)))

qName :: Lens' DNSQuery Name
qName = lens getter setter
  where
    getter dnsQuery = Name $ go 12
      where
        go :: Int -> [ByteString]
        go offset =
          let len = fromIntegral $ dnsQuery ^?! bytes . ix offset
           in if len == 0
                then []
                else
                  B.pack
                    ( map
                        (B.index (dnsQuery ^. bytes))
                        [offset + 1 .. offset + len]
                    )
                    : go (offset + len + 1)
    setter dnsQuery (Name q) =
      let (header, rest) = B.splitAt 12 (dnsQuery ^. bytes)
          qnameBytes =
            foldr
              (\part acc -> B.cons (fromIntegral $ B.length part) part <> acc)
              (B.singleton 0)
              q
          totalLen = B.length qnameBytes
          rest' = B.drop totalLen rest
       in DNSQuery $ header <> qnameBytes <> rest'

qNameLen :: Name -> Int
qNameLen (Name q) = sum (map (P.succ . B.length) q) + 1

qType :: Lens' DNSQuery Word16
qType = word16Lens [const 12, qNameLen . view qName]

qClass :: Lens' DNSQuery Word16
qClass = word16Lens [const 12, qNameLen . view qName, const 2]
