module Main where

import Control.Error
import Control.Lens (_head)
import Data.Default
import Data.IP
import Network.Socket
import Network.Socket.ByteString
import Parser
import RIO hiding (id)
import qualified RIO.ByteString as B
import Types

main :: IO ()
main = pure ()

testPacket :: Int -> IO ByteString
testPacket i = B.readFile ("dns-server-tests/test" <> show i <> "/packet")

udpSocket :: MonadIO m => m Socket
udpSocket = liftIO $ socket AF_INET Datagram defaultProtocol

queryServer :: Name -> IPv4 -> ExceptT Text IO Query
queryServer nm ip = do
  s <- udpSocket
  let query = queryToByteString (def & question . _head . qname .~ nm)
  liftIO $ sendAllTo s query (SockAddrInet 53 (toHostAddress ip))
  response <- timeout (10 ^ 6) (recv s 1024) !? (tshow ip <> " timed out")
  hoistEither $ parseQuery response
