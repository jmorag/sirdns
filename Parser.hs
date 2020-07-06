module Parser where

import Control.Monad (replicateM)
import Data.Bit
import Data.Bits
import Data.IP
import Data.Semigroup
import RIO hiding (id)
import qualified RIO.ByteString as B
import RIO.State
import RIO.Writer
import Types

newtype Parser a = Parser {getParser :: ByteString -> Int -> (a, Int)}
  deriving (Functor)

instance Applicative Parser where
  pure x = Parser \_ offset -> (x, offset)
  (Parser f) <*> (Parser x) = Parser \bytes o0 ->
    let (g, o1) = f bytes o0
        (y, o2) = x bytes o1
     in (g y, o2)

instance Monad Parser where
  (Parser x) >>= f = Parser \bytes o0 ->
    let (y, o1) = x bytes o0
     in getParser (f y) bytes o1

instance MonadReader ByteString Parser where
  ask = Parser \bytes offset -> (bytes, offset)
  local f (Parser x) = Parser (x . f)

instance MonadState Int Parser where
  state = Parser . const

pState :: (MonadState Int m, MonadReader ByteString m) => m (ByteString, Int)
pState = do
  bytes <- ask
  offset <- get
  pure (bytes, offset)

byte :: (MonadState Int m, MonadReader ByteString m) => m Word8
byte = do
  (bytes, offset) <- pState
  pure $ B.index bytes offset

incr :: (MonadState Int m) => m ()
incr = modify (+ 1)

word16P :: Parser Word16
word16P = do
  b0 <- fromIntegral <$> byte <* incr
  b1 <- fromIntegral <$> byte <* incr
  pure $ shiftL b0 8 + b1

qrP :: Parser Bit
qrP = do
  (bytes, offset) <- pState
  pure $ Bit (testBit (B.index bytes offset) 7)

opCodeP :: Parser Word8
opCodeP = do
  b <- byte
  pure $ shiftR b 3 `mod` 16

aaP :: Parser Bit
aaP = Bit . flip testBit 2 <$> byte

tcP :: Parser Bit
tcP = Bit . flip testBit 1 <$> byte

rdP :: Parser Bit
rdP = Bit . flip testBit 0 <$> byte <* incr

raP :: Parser Bit
raP = Bit . flip testBit 7 <$> byte

zP :: Parser Word8
zP = fmap (\b -> shiftR b 4 `mod` 8) byte

rCodeP :: Parser Word8
rCodeP = fmap (`mod` 16) byte <* incr

nameP :: Parser Name
nameP = do
  (nm, Max offset) <- runWriterT go
  Name nm <$ put offset
  where
    go :: WriterT (Max Int) Parser [ByteString]
    go = do
      len <- byte
      let len' = fromIntegral len
      if  | len == 0 -> gets (+ 1) >>= write >> pure []
          | shiftR len 6 == 3 -> pState >>= \(bytes, offset) -> do
            let offset' =
                  fromIntegral $
                    shiftL (len `mod` (2 ^ 5)) 8 + B.index bytes (offset + 1)
            put offset' >> tell (Max (offset + 2)) >> go
          | otherwise -> pState >>= \(bytes, offset) -> do
            let part = B.take len' (B.drop (offset + 1) bytes)
                offset' = offset + len' + 1
            write offset' >> fmap (part :) go
    write x = put x >> tell (Max x)

word32P :: Parser Word32
word32P = do
  b0 <- flip shiftR 24 <$> byte <* incr
  b1 <- flip shiftR 16 <$> byte <* incr
  b2 <- flip shiftR 8 <$> byte <* incr
  b3 <- byte <* incr
  pure $ sum (map fromIntegral [b0, b1, b2, b3])

headerP :: Parser Header
headerP =
  Header
    <$> word16P
      <*> qrP
      <*> opCodeP
      <*> aaP
      <*> tcP
      <*> rdP
      <*> raP
      <*> zP
      <*> rCodeP
      <*> word16P
      <*> word16P
      <*> word16P
      <*> word16P

questionP :: Parser Question
questionP = do
  _qname <- nameP
  _qtype <- word16P <&> \case
    1 -> A
    2 -> NAMESERVER
    5 -> CNAME
    28 -> AAAA
    other -> error $ "QTYPE " <> show other <> " not supported"
  word16P >>= \case
    1 -> pure (Question {..})
    other -> error $ "QCLASS " <> show other <> " not supported"

recordP :: Parser Record
recordP = do
  _name <- nameP
  _type <- word16P
  _class <- word16P
  _ttl <- word32P
  _rdlength <- word16P
  _rdata <- case _type of
    1 -> ARecord . toIPv4w <$> word32P
    2 -> NameServer <$> nameP
    5 -> CName <$> nameP
    28 -> AAAARecord . toIPv6 . map fromIntegral <$> replicateM 8 word16P
    other -> error $ "RDATA type " <> show other <> " not supported"
  pure $ Record {..}

queryP :: Parser Query
queryP = do
  _header <- headerP
  _question <- replicateM' (_qdcount _header) questionP
  _answer <- replicateM' (_ancount _header) recordP
  _authority <- replicateM' (_nscount _header) recordP
  _additional <- replicateM' (_arcount _header) recordP
  pure $ Query {..}
  where
    replicateM' = replicateM . fromIntegral

parseQuery :: ByteString -> Query
parseQuery bytes = fst $ getParser queryP bytes 0
