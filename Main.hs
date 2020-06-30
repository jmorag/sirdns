module Main where

import Control.Lens
import Data.Bit
import Data.Bits
import Data.IP
import Network.Socket
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
import qualified RIO.Text as T
import qualified Prelude as P

main :: IO ()
main = pure ()

testPacket :: Int -> IO ByteString
testPacket i = B.readFile ("dns-server-tests/test" <> show i <> "/packet")

sock :: IO Socket
sock = socket AF_INET Datagram defaultProtocol

