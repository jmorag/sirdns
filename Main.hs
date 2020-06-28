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

id :: Lens' DNSQuery Word16
id = lens getter setter
  where
    getter dnsHeader = let firstByte = fromIntegral $ B.index (dnsHeader ^. bytes) 0
                           secondByte = fromIntegral $ B.index (dnsHeader ^. bytes) 1
                       in shiftL firstByte 8 + secondByte
    setter dnsHeader w = let firstByte = shiftR w 8 & fromIntegral
                             secondByte = fromIntegral w
                         in set (bytes . ix 0) firstByte dnsHeader & set (bytes . ix 1) secondByte

qr :: Lens' DNSQuery Bool
qr = lens getter setter
  where
    getter dnsHeader = B.index (dnsHeader ^. bytes) 2 & \byte -> testBit byte 0
    setter dnsHeader b =
      over
        (bytes . ix 2)
        (\byte -> if b then setBit byte 0 else clearBit byte 0)
        dnsHeader
