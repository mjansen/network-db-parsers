module Data.NetworkDB.NetgroupEntry
  ( NetgroupEntry(..)
  , readNetgroupFile
  , readNetgroupFileContent
  ) where

import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString.Char8 as P
import Debug.Trace

import Data.NetworkDB.Helpers.ByLineParser

isSpace :: Char -> Bool
isSpace c = (c == ' ') || (c == '\t')

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

-- | netgroup file

parseGroupName = do
  name <- P.takeWhile (\c -> isAlphaNumeric c || (c == '_'))
  if B.length name == 0 then fail "no parse" else return name

parseHostName = do
  name <- P.takeWhile (\c -> isAlphaNumeric c || (c == '-') || (c == '.'))
  if B.length name == 0 then fail "no parse" else return name

data NetgroupEntry = NetgroupEntry
  { nge_name :: B.ByteString
  , nge_components :: [NetgroupComponent]
  } deriving (Eq, Ord, Show, Read)

parseNetgroupEntry = do
  name <- parseGroupName
  entries <- P.many1 (skipSpace >> parseNetgroupComponent)
  skipSpace
  return $ NetgroupEntry name entries
  
data NetgroupComponent = Group B.ByteString | Host B.ByteString
     deriving (Eq, Ord, Show, Read)
                            
parseNetgroupComponent = P.choice [ P.try parseTriplet, fmap Group parseGroupName ]
  
parseTriplet = do
  P.char '('
  skipSpace
  hName <- parseHostName
  skipSpace
  P.char ','
  skipSpace
  P.char ','
  skipSpace
  P.char ')'
  return $ Host hName
  
-- | Interface

readNetgroupFileContent :: B.ByteString -> [NetgroupEntry]
readNetgroupFileContent = parseContentLines parseNetgroupEntry
 
readNetgroupFile :: String -> IO [NetgroupEntry]
readNetgroupFile = parseFileLines parseNetgroupEntry
