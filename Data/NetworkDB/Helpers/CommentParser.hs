module Data.NetworkDB.Helpers.CommentParser
  ( removeComments
  , removeCommentsAndGiveLines
  ) where

-- | Some configuration file formats define comments to begin from a
-- '#' symbol and extend to the end of the line.

import qualified Data.ByteString.Char8            as B
import qualified Data.Attoparsec.ByteString.Char8 as P

-- | First Parser stage: filter comments
    
commentFilter = do
  text    <- P.takeWhile (/= '#')
  comment <- P.takeWhile (/= '\n')
  return (text, comment)
  
repeatedCommentFilter = fmap (B.concat . map fst) $ P.manyTill commentFilter P.endOfInput
  
removeComments :: B.ByteString -> B.ByteString
removeComments content = 
  case P.parseOnly repeatedCommentFilter content of
    Right content' -> content'
    Left _         -> error "this is not possible 830174839"  -- the comment parser cannot fail

-- | Line Structured
-- If a configuration file is line structured,
-- then we can avoid recombining things, we just produce a list of
-- lines.

getLineWithoutComment = do
  text <- P.takeWhile (\c -> (c /= '#') && (c /= '\n'))
  _    <- P.takeWhile (/= '\n')
  _    <- P.choice [P.char '\n' >> return (), P.endOfInput]
  return text
  
getLinesWithoutComment = P.manyTill getLineWithoutComment P.endOfInput

removeCommentsAndGiveLines :: B.ByteString -> [B.ByteString]
removeCommentsAndGiveLines content =
  case P.parseOnly getLinesWithoutComment content of
    Right ls -> ls
    Left _   -> error "this is not possible 789174839104" -- the comment parse cannot fail
