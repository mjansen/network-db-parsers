module Data.NetworkDB.Helpers.ByLineParser
  ( parseFileLines
  , parseContentLines
  ) where

import Data.Maybe
import qualified Data.ByteString.Char8            as B
import qualified Data.Attoparsec.ByteString.Char8 as P

import Data.NetworkDB.Helpers.CommentParser

-- | Type for different types of line

data LineType a = ParseOK a | Blank | ParseError B.ByteString
     deriving Eq

unwrap :: LineType a -> Maybe a
unwrap (ParseOK x) = Just x
unwrap _           = Nothing

-- | Character Predicates

isSpace :: Char -> Bool
isSpace c = (c == ' ') || (c == '\t')

skipSpace = P.takeWhile isSpace

parseEndOfLine = P.choice [ P.char '\n' >> return (), P.endOfInput ]

-- | Lines are of 3 different types:

parseBlank     = skipSpace >> parseEndOfLine >> return Blank

parseError     = do
  rest <- P.takeWhile (/= '\n')
  parseEndOfLine
  return $ ParseError rest

parseLine parseEntry = P.choice [ P.try parseBlank
                                , fmap ParseOK $ P.try (parseEntry >>= (\result -> parseEndOfLine >> return result))
                                , parseError ]

parseFile parseEntry = P.manyTill (parseLine parseEntry) P.endOfInput

-- | Interface

parseFileLines :: P.Parser a -> String -> IO [a]
parseFileLines parseEntry fileName = fmap (parseContentLines parseEntry) . B.readFile $ fileName

parseContentLines :: P.Parser a -> B.ByteString -> [a]
parseContentLines parseEntry content =
  case P.parseOnly (parseFile parseEntry) . removeComments $ content of
    Right rs     -> catMaybes . map unwrap $ rs
    Left message -> error "this should not happen"

--

isBlank      (Blank)        = True
isBlank      _              = False

isParseOK    (ParseOK _)    = True
isParseOK    _              = False

isParseError (ParseError _) = True
isParseError _              = False
