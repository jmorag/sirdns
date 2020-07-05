module Types where

import RIO
import Control.Lens (makeLenses, makePrisms)
import Data.Bit
import Data.IP

data Header
  = Header
      { _id :: !Word16,
        _qr :: !Bit,
        _opcode :: !Word8,
        _aa :: !Bit,
        _tc :: !Bit,
        _rd :: !Bit,
        _ra :: !Bit,
        _z :: !Word8,
        _rcode :: !Word8,
        _qdcount :: !Word16,
        _ancount :: !Word16,
        _nscount :: !Word16,
        _arcount :: !Word16
      }
  deriving (Show)

makeLenses ''Header

data CLASS = InternetAddress -- others not supported
  deriving (Show)

data TYPE = A | CNAME | NAMESERVER
  deriving (Show)

makePrisms ''TYPE

newtype Name = Name {_labels :: [ByteString]}
  deriving (Show)

makeLenses ''Name

data Question
  = Question
      { _qname :: !Name,
        _qtype :: !TYPE,
        _qclass :: !CLASS
      }
  deriving (Show)

makeLenses ''Question

data RData = ARecord IPv4 | CName Name | NameServer Name
  deriving (Show)

makePrisms ''RData

data Record
  = Record
      { _name :: !Name,
        _class' :: !CLASS,
        _ttl :: !Word32,
        _rdata :: !RData
      }
  deriving (Show)

makeLenses ''Record

newtype Answer = Answer { _unAnswer :: [Record] }
newtype Authority = Authority { _unAuthority :: [Record] }
newtype Additional = Additional { _unAdditional :: [Record] }
makeLenses ''Answer
makeLenses ''Authority
makeLenses ''Additional

data Query
  = Query
      { _header :: !Header,
        _question :: !Question,
        _answer :: !Answer,
        _authority :: !Authority,
        _additional :: !Additional
      }

makeLenses ''Query
