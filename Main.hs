module Main where

import Control.Lens
import Data.Bits
import Network.Socket
import RIO hiding
  ( (%~),
    (.~),
    ASetter,
    ASetter',
    Getting,
    Lens,
    Lens',
    SimpleGetter,
    (^.),
    (^..),
    (^?),
    lens,
    over,
    preview,
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

-- data DNSQuery = DNSQuery {
--   id :: Word16,
--   qr :: Bool,
--   opCode :: Word8 -- 4 bits

-- }

newtype DNSQuery = DNSQuery {_bytes :: ByteString}

bytes :: Lens' DNSQuery ByteString
bytes = lens _bytes (\x y -> x {_bytes = y})

bytesLens :: Int -> Lens' DNSQuery Word16
bytesLens byte = lens getter setter
  where
    getter dnsHeader = let firstByte = fromIntegral $ B.index (dnsHeader ^. bytes) byte
                           secondByte = fromIntegral $ B.index (dnsHeader ^. bytes) (byte + 1)
                       in shiftL firstByte 8 + secondByte
    setter dnsHeader w = let firstByte = shiftR w 8 & fromIntegral
                             secondByte = fromIntegral w
                         in set (bytes . ix byte) firstByte dnsHeader & set (bytes . ix (byte + 1)) secondByte

id :: Lens' DNSQuery Bool
id = bytesLens 0

qdCount :: Lens' DNSQuery Bool
qdCount = bytesLens 2

anCount :: Lens' DNSQuery Bool
anCount = bytesLens 3

nsCount :: Lens' DNSQuery Bool
nsCount = bytesLens 4

arCount :: Lens' DNSQuery Bool
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
