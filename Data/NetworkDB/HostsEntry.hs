module Data.NetworkDB.HostsEntry
  ( HostEntry(..)
  , readHostsFileContent
  , readHostsFile
  , hfLookup
  ) where

import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString.Char8 as P
import Debug.Trace

import Data.NetworkDB.Helpers.ByLineParser

data IPAddress = IPAddress Int Int Int Int
               deriving (Eq, Ord, Show, Read)

ipAddressToString (IPAddress a b c d) = show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d

data HostEntry = HostEntry
  { ipAddress :: IPAddress
  , names     :: [B.ByteString]
  } deriving (Eq, Ord, Show, Read)

parseEndOfLine = P.char '\n'

parseHostsLine = do
  addr <- parseIPAddress
  names <- P.many1 (parseSpace >> parseName)
  return $ HostEntry addr names

-- | Character Pridicates

isSpace :: Char -> Bool
isSpace c = (c == ' ') || (c == '\t')

isSpaceOrLineFeed :: Char -> Bool
isSpaceOrLineFeed c = isSpace c || (c == '\n')

isAlpha = P.isAlpha_ascii

isDigit = P.isDigit

isAlphaNumeric c = isAlpha c || isDigit c

isNameChar :: Char -> Bool
isNameChar c = isAlphaNumeric c || (c == '-') || (c == '.')

-- |

isHostNameStrict :: B.ByteString -> Bool
isHostNameStrict name =
  let l = B.length name 
  in ( l > 0 
       && isAlpha (B.index name 0) 
       && B.all isNameChar name
       && isAlphaNumeric (B.index name (l - 1))
     )

-- | Basic Lexical Helpers

skipSpace = P.takeWhile isSpace

parseSpace = P.satisfy isSpace >> skipSpace

parseName = P.takeWhile (not . isSpaceOrLineFeed)

parseNameStrict = do
  name <- parseName
  if isHostNameStrict name
    then return name
    else fail ""

parseNames = parseName `P.sepBy` parseSpace

parseIPAddress = do
  octets <- P.decimal `P.sepBy` P.char '.'
  if length octets /= 4 || any (> 255) octets
    then fail "cannot parse ip address"
    else return $ IPAddress (octets!!0) (octets!!1) (octets!!2) (octets!!3)

-- | Interface

readHostsFileContent :: B.ByteString -> [HostEntry]
readHostsFileContent = parseContentLines parseHostsLine
 
readHostsFile :: String -> IO [HostEntry]
readHostsFile = parseFileLines parseHostsLine

hfLookup :: B.ByteString -> [HostEntry] -> B.ByteString
hfLookup name rs =
  let xs = filter ((name `elem`) . names) rs
  in case xs of
    []    -> name
    (r:_) -> B.pack . ipAddressToString . ipAddress $ r
