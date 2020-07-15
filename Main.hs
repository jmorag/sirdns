module Main where

import Algorithm.Search (dfsM)
import Control.Error
import Control.Lens
import Data.Default
import Data.Foldable (find)
import Data.IP
import Network.Socket
import Network.Socket.ByteString
import Parser
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
    id,
    lens,
    over,
    preview,
    set,
    sets,
    to,
    view,
  )
import qualified RIO.ByteString as B
import System.Environment (getArgs)
import Text.Pretty.Simple
import Types
import Prelude (putStrLn)

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

findAddr :: Name -> IO (Maybe IPv4)
findAddr nm = do
  Right initial <- runExceptT $ queryServer nm "199.9.14.201"
  path <- dfsM nextStates done initial
  pure $ lastOf (_Just . traversed . answer . traversed . rdata . _ARecord) path
  where
    nextStates :: Query -> IO [Query]
    nextStates q =
      rights
        <$> traverse
          (runExceptT . queryServer nm)
          (q ^.. records . rdata . _ARecord)
    done :: Query -> IO Bool
    done q = pure $ q ^. header . aa == 1
