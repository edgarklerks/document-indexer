module Etags.Parser where

import Text.Trifecta
import Control.Applicative
import Control.Monad
import Data.Word
import Data.Monoid


{-- Format of a etags file:
<\x0c>
srcfile,sizeoffileinbytes
tagdefinition<\x7f>tagname<\x01>linenumber,byte_offset
 ---}

data Etag = Etag {
      srcfile :: FilePath,
      tagsize :: Word64,
      tagdefinitions :: [TagDef]
 } deriving Show

data TagDef = TagDef {
            tagdefinition :: String,
            tagname :: String,
            linenumber :: Word64,
            byte_offset :: Word64
    } deriving Show

readEtagFile :: FilePath -> IO (Maybe [Etag])
readEtagFile fp = parseFromFile parseEtags fp

ppEtag :: Etag -> IO ()
ppEtag e = do
       putStrLn "\x0c"
       putStrLn (srcfile e <> "," <> show (tagsize e))
       let def = tagdefinitions e
       forM_ def $ \d ->
        putStrLn (tagdefinition d <> "\x7f" <> tagname d <> "\x01" <> show ( linenumber d ) <> ","  <> show (byte_offset d) )

-- | This is somewhat ugly, but it parses a bigger class of files than only ETAGS files, so it works quite ok.
parseEtags :: Parser [Etag]
parseEtags = do
         string "\x0c\n"
         parseETag `sepEndBy` (void (string "\x0c\n") <|> void (char '\n') <|> eof)

parseETag :: Parser Etag
parseETag = do
       fp <- some (alphaNum <|> oneOf "./-_+%~()# ")
       char ','
       bytesize <- some digit
       char '\n'
       t <- Etag fp (read bytesize) <$> (parseTagdefs fp)
       return t

parseTagdefs :: FilePath -> Parser [TagDef]
parseTagdefs fp = parseDef
     where parseDef = parseTagdef fp `sepEndBy` (char '\n') <?> "Couldn't parse definition"

parseTagdef :: FilePath -> Parser TagDef
parseTagdef fp = do
         x <- some $ nonep ('\x7f'/=) <?> ("Tag definition" <> fp )
         char '\x7f'
         y <- some $ satisfy (/='\x01') <?> "Tag name"
         char '\x01'
         ln <- some digit <?> "Line number"
         char ','
         bo <- some digit <?> "Byte offset"
         return (TagDef x y (read ln) (read bo))

-- | Test if we have have a valid character (^L may not be followed by a \n)
parseChars p = satisfy (\x -> p x && x /= '\x0c') <|> (try $ satisfy (=='\x0c') <* notFollowedBy  (char '\n'))
nonep p = parseChars p -- satisfy (\x -> p x && x /= '\x0c')
