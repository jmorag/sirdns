module Main where

import Control.Error
import Control.Lens (_head, (.~), _Just, (^?), (^..), traversed)
import Data.Default
import Data.Foldable (find)
import Data.IP
import Data.Tree
import Network.Socket
import Network.Socket.ByteString
import Parser
import RIO hiding (id)
import qualified RIO.ByteString as B
import Types
import System.Environment (getArgs)
import Prelude (putStrLn)
import Text.Pretty.Simple

main :: IO ()
main = do
  [addr] <- getArgs
  ip <- findAddr (fromString addr)
  pPrint ip

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

genTree :: Name -> IO (Tree Query)
genTree nm = unfoldTreeM go "199.9.14.201" where
  go :: IPv4 -> IO (Query, [IPv4])
  go ip = do
    query <- runExceptT (queryServer nm ip)
    case query of
      Left err -> error (show err)
      Right q -> pure (q, q ^.. additional . traversed . rdata . _ARecord)

findIP :: Tree Query -> Maybe IPv4
findIP tree =
  let query = find (\q -> q ^. header . aa == 1) tree in
    query ^? _Just . answer . _head . rdata . _ARecord

findAddr :: Name -> IO IPv4
findAddr nm = do
  tree <- genTree nm
  putStrLn (drawTree (fmap show tree))
  let (Just ip) = findIP tree
  pure ip
