module Main where

import Network.Socket
import Network.Socket.ByteString
import Control.Lens
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
import Prelude (print)

import Types

main :: IO ()
main = pure ()

testPacket :: Int -> IO DNSQuery
testPacket i = DNSQuery <$> B.readFile ("dns-server-tests/test" <> show i <> "/packet")

udpSocket :: IO Socket
udpSocket = socket AF_INET Datagram defaultProtocol

testOutgoing :: IO (DNSQuery, DNSQuery)
testOutgoing = do
  s <- udpSocket
  let query = mkQuery "google.com"
  sendAllTo s (query ^. bytes) (SockAddrInet 53 (tupleToHostAddress (199,9,14,201)))
  (resp, addr) <- recvFrom s 1024
  close s
  print addr
  pure (query, DNSQuery resp)

localhost :: PortNumber -> SockAddr
localhost port = SockAddrInet port (tupleToHostAddress (127,0,0,1))
