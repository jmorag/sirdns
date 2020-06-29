module Main where

import Control.Lens
import Data.Bits
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
import qualified Prelude as P

main :: IO ()
main = P.putStrLn "Hello, Haskell!"

sock :: IO Socket
sock = socket AF_INET Datagram defaultProtocol

newtype DNSQuery = DNSQuery {_bytes :: ByteString}

bytes :: Lens' DNSQuery ByteString
bytes = lens _bytes (\x y -> x {_bytes = y})

bytesLens :: Int -> Lens' DNSQuery Word16
bytesLens byte = lens getter setter
  where
    getter dnsHeader =
      let firstByte = fromIntegral $ B.index (dnsHeader ^. bytes) byte
          secondByte = fromIntegral $ B.index (dnsHeader ^. bytes) (byte + 1)
       in shiftL firstByte 8 + secondByte
    setter dnsHeader w =
      let firstByte = shiftR w 8 & fromIntegral
          secondByte = fromIntegral w
       in set (bytes . ix byte) firstByte dnsHeader & set (bytes . ix (byte + 1)) secondByte


id :: Lens' DNSQuery Word16
id = bytesLens 0

qdCount :: Lens' DNSQuery Word16
qdCount = bytesLens 2

anCount :: Lens' DNSQuery Word16
anCount = bytesLens 3

nsCount :: Lens' DNSQuery Word16
nsCount = bytesLens 4

arCount :: Lens' DNSQuery Word16
arCount = bytesLens 5

bitLens :: Int -> Int -> Lens' DNSQuery Bool
bitLens byte bit = lens getter setter
  where
    getter dnsHeader = B.index (dnsHeader ^. bytes) byte & \byte' -> testBit byte' bit
    setter dnsHeader b =
      over
        (bytes . ix byte)
        (\byte' -> if b then setBit byte' bit else clearBit byte' bit)
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
