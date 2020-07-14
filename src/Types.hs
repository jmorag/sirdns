{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens (makeLenses, makePrisms)
import Data.Bit
import Data.ByteString.Internal (c2w)
import Data.Default
import Data.IP
import RIO
import qualified RIO.ByteString as B
import qualified RIO.Text as T

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
  deriving (Show, Eq, Generic)

makeLenses ''Header

instance Default Header where
  def =
    Header
      { _id = 1337,
        _qr = 0,
        _opcode = 0,
        _aa = 0,
        _tc = 0,
        _rd = 1,
        _ra = 0,
        _z = 0,
        _rcode = 0,
        _qdcount = 1,
        _ancount = 0,
        _nscount = 0,
        _arcount = 0
      }

data TYPE = A | CNAME | NAMESERVER | AAAA
  deriving (Show, Eq, Generic)

makePrisms ''TYPE

newtype Name = Name {_labels :: [ByteString]}
  deriving (Show, Eq, Generic)

makeLenses ''Name

instance IsString Name where
  fromString = Name . B.split (c2w '.') . encodeUtf8 . T.pack

data Question
  = Question
      { _qname :: !Name,
        _qtype :: !TYPE
      }
  deriving (Show, Eq, Generic)

makeLenses ''Question

data RData = ARecord IPv4 | CName Name | NameServer Name | AAAARecord IPv6
  deriving (Show, Eq, Generic)

makePrisms ''RData

data Record
  = Record
      { _name :: !Name,
        _ttl :: !Word32,
        _rdlength :: !Word16,
        _rdata :: !RData
      }
  deriving (Show, Eq, Generic)

makeLenses ''Record

data Query
  = Query
      { _header :: !Header,
        _question :: ![Question],
        _answer :: ![Record],
        _authority :: ![Record],
        _additional :: ![Record]
      }
  deriving (Show, Eq, Generic)

makeLenses ''Query

instance Default Query where
  def = Query def [Question "google.com" A] [] [] []
