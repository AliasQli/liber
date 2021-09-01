{-# LANGUAGE QuasiQuotes        #-}

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module MegaParser.Parse (parseScript, encodeScript, Script(..), Record(..)) where

import           Control.Monad              (void)
import           Data.Binary
import           Data.Char                  (isPrint)
-- import           Data.Encoding.UTF8
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Data.Void                  (Void)
import           GHC.Generics
import           Orphans                    ()
import           System.IO
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


-- | The overall parser type.
type Parser = Parsec Void Text

-- | 'Pieces' on a chess board.
type Pieces = Map Text Text

data Record
  = Record (Maybe Text) (Vector Text)
  deriving (Show, Generic, Binary)

newtype Script = Script (Vector Record)
  deriving (Generic, Show)
  deriving anyclass (Binary)

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

emptyLine :: Parser ()
emptyLine = sc <* eol

-- | @lexeme = (<* sc)@
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

nameP :: Parser Text
nameP = T.pack <$> some (alphaNumChar <|> oneOf ['-', '_']) <?> "name"

trim :: Text -> Text
trim = T.dropWhile (==' ') . T.dropWhileEnd (== ' ')

senP :: Parser Text
senP = T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))
   <|> trim . T.pack <$>
          someTill
            (satisfy (\c -> isPrint c && c /= '#'))
            (lookAhead $ void eol <|> void (char '#') <|> eof)

topSenP :: Parser Text
topSenP = T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))
      <|> (\x -> trim . T.pack . (x:)) <$>
              satisfy (\c -> isPrint c && c /= '#' && c /= '`') <*>
              manyTill
                (satisfy (\c -> isPrint c && c /= '#'))
                (lookAhead $ void eol <|> void (char '#') <|> eof)

piecesP :: Parser Pieces
piecesP = do
  string "Pieces" *> some emptyLine
  nm1 <- lexeme nameP
  pos <- L.indentLevel
  str1 <- lexeme senP
  eol
  ts <- many ((,) <$> lexeme nameP <* L.indentGuard sc EQ pos <*> lexeme senP <* eol)
  many emptyLine
  return $ M.fromList $ (nm1, str1):ts

narrateP :: Parser (Vector Record)
narrateP = V.singleton . Record Nothing . V.fromList <$> some (lexeme topSenP <* eol) <* many emptyLine

speakP :: Pieces -> Parser (Vector Record)
speakP pieces = do
  nm <- nameP
  let name = fromMaybe nm $ M.lookup nm pieces
  char ' ' <|> char '\t'
  sc
  pos <- L.indentLevel
  let p = Record (Just name) . V.fromList <$> some (L.indentGuard sc EQ pos *> lexeme senP <* eol)
  V.fromList <$> (p `sepEndBy1` (sc *> eol *> scn))

  -- optional (lexeme senP) <&> \case
  --   Nothing   -> L.IndentSome Nothing    (return . Record (Just name) . V.fromList)           senP
  --   Just sen1 -> L.IndentMany (Just pos) (return . Record (Just name) . V.fromList . (sen1:)) senP

recordsP :: Pieces -> Parser (Vector Record)
recordsP pieces = do
  string "Records"
  some emptyLine
  V.concat <$> some (try (speakP pieces) <|> narrateP)

allP :: Parser (Vector Record)
allP = do
  scn
  pieces <- option M.empty (try piecesP)
  records <- recordsP pieces
  eof
  return records

parseScript :: Text -> Either (ParseErrorBundle Text Void) Script
parseScript t = Script <$> runParser allP "script file" (t <> "\n")

encodeScript :: FilePath -> FilePath -> IO ()
encodeScript inP outP = do
  -- let ?enc = UTF8
  -- inF <- T.readFile inP
  h <- openFile inP ReadMode
  hSetEncoding h utf8
  inF <- T.hGetContents h
  -- inF <- withBinaryFile inP ReadMode T.hGetContents
  case parseScript inF of
    Left e    -> putStr (errorBundlePretty e)
    Right scr -> encodeFile outP scr
