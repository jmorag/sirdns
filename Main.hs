module Main where

import Control.Lens
import Data.Bit
import Data.Bits
import Data.IP
import Network.Socket
import Network.Socket.ByteString
import RIO hiding (id)
import qualified RIO.ByteString as B
import qualified RIO.Text as T
import qualified Prelude as P

import Types
import Parser

main :: IO ()
main = pure ()

testPacket :: Int -> IO ByteString
testPacket i = B.readFile ("dns-server-tests/test" <> show i <> "/packet")

sock :: IO Socket
sock = socket AF_INET Datagram defaultProtocol

udpSocket :: IO Socket
udpSocket = socket AF_INET Datagram defaultProtocol

testOutgoing :: IO (ByteString, ByteString)
testOutgoing = do
  s <- udpSocket
  query <- testPacket 1
  sendAllTo s query (SockAddrInet 53 (tupleToHostAddress (199,9,14,201)))
  (resp, addr) <- recvFrom s 1024
  close s
  P.print addr
  pure (query, resp)
