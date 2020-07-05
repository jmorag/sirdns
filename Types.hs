{-# LANGUAGE TemplateHaskell #-}
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

data TYPE = A | CNAME | NAMESERVER | AAAA
  deriving (Show)

makePrisms ''TYPE

newtype Name = Name { _labels :: [ByteString] }
  deriving (Show)

makeLenses ''Name

data Question
  = Question
      { _qname :: !Name,
        _qtype :: !TYPE
      }
  deriving (Show)

makeLenses ''Question

data RData = ARecord IPv4 | CName Name | NameServer Name | AAAARecord IPv6
  deriving (Show)

makePrisms ''RData

data Record
  = Record
      { _name :: !Name,
        _ttl :: !Word32,
        _rdata :: !RData
      }
  deriving (Show)

makeLenses ''Record

data Query
  = Query
      { _header :: !Header,
        _question :: ![Question],
        _answer :: ![Record],
        _authority :: ![Record],
        _additional :: ![Record]
      }
  deriving (Show)

makeLenses ''Query
