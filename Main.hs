module Main where

import Network.Socket
import RIO
import qualified RIO.ByteString as B
import qualified RIO.Text as T

import Types

main :: IO ()
main = pure ()

testPacket :: Int -> IO DNSQuery
testPacket i = DNSQuery <$> B.readFile ("dns-server-tests/test" <> show i <> "/packet")

sock :: IO Socket
sock = socket AF_INET Datagram defaultProtocol
