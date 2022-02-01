module HW3.Parser
  where
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixN, InfixR), makeExprParser)
import qualified Data.ByteString as BS
import Data.Char (digitToInt, isAlpha, isAlphaNum)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Void (Void)
import Data.Word (Word8)
import HW3.Base (HiAction (..), HiExpr (HiExprApply, HiExprDict, HiExprRun, HiExprValue),
                 HiFun (..), HiValue (..), convertToAction, convertToOperation, listOfAllOperators,
                 listOfAllSpecialActions)
import Text.Megaparsec (MonadParsec (eof, try), ParseErrorBundle, Parsec, choice, errorBundlePretty,
                        many, manyTill, notFollowedBy, optional, runParser, satisfy, sepBy,
                        sepEndBy, (<|>))
import Text.Megaparsec.Byte.Lexer (lexeme)
import Text.Megaparsec.Char (char, hexDigitChar, space, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (
  do
    res <- parser3
    eof
    return res
 ) ""

getErrorMessage :: ParseErrorBundle String Void -> String
getErrorMessage = errorBundlePretty

operations :: [[Operator Parser HiExpr]]
operations = [
  [
    binary' divString HiFunDiv InfixL,
    binary "*" HiFunMul InfixL
  ],
  [
    binary "+" HiFunAdd InfixL,
    binary "-" HiFunSub InfixL
  ],
  [
    binary ">=" HiFunNotGreaterThan InfixN,
    binary "<=" HiFunNotLessThan InfixN,
    binary "<" HiFunLessThan InfixN,
    binary ">" HiFunGreaterThan InfixN,
    binary "==" HiFunEquals InfixN,
    binary "/=" HiFunNotEquals InfixN
  ],
  [
    binary "&&" HiFunAnd InfixR
  ],
  [
    binary "||" HiFunOr InfixR
  ]]

divString :: Parser String
divString = try (do
  s <- string "/"
  notFollowedBy (string "=")
  return s)

oneCharWithoutContinuation :: String -> String -> Parser String
oneCharWithoutContinuation st notExp = try (do
  s <- string st
  notFollowedBy (string notExp)
  return s)

oneCharWithoutContinuation' :: String -> String -> Parser String
oneCharWithoutContinuation' st notExp = try (do
  s <- string st
  notFollowedBy (space *> string notExp)
  return s)

postfix ::
  String ->
  (Parser (HiExpr -> HiExpr)
    -> Operator Parser HiExpr) ->
  Operator Parser HiExpr
postfix name pr = pr (HiExprRun <$ string name)

binary ::
  String ->
  HiFun ->
  (Parser (HiExpr -> HiExpr -> HiExpr)
    -> Operator Parser HiExpr) ->
  Operator Parser HiExpr
binary name = binary' (string name)

binary' ::
  Parser String ->
  HiFun ->
  (Parser (HiExpr -> HiExpr -> HiExpr)
    -> Operator Parser HiExpr) ->
  Operator Parser HiExpr
binary' name f pr = pr (
  (\a b -> HiExprApply (HiExprValue . HiValueFunction $ f) [a, b]) <$ name)

parserSequence:: Char -> Char -> Parser HiExpr -> Parser [HiExpr]
parserSequence ob cb pTerm = do
  _ <- char ob
  _ <- space
  h <- optional pTerm
  _ <- space
  t <- many (
    do
      _ <- char ','
      _ <- space
      pTerm)
  _ <- space
  _ <- char cb
  return $ let
    args = case h of
      Nothing -> []
      Just h' -> h':t in
    args

parserSequence':: Parser String -> Parser String -> Parser HiExpr -> Parser [HiExpr]
parserSequence' ob cb pTerm = do
  _ <- ob
  _ <- space
  h <- optional pTerm
  _ <- space
  t <- many (
    do
      _ <- char ','
      _ <- space
      pTerm)
  _ <- space
  _ <- cb
  return $ let
    args = case h of
      Nothing -> []
      Just h' -> h':t in
    args


parserByte :: Parser Word8
parserByte = do
  d1 <- hexDigitChar
  d2 <- hexDigitChar
  return $ toEnum (16 * digitToInt d1 + digitToInt d2)

parserByteArray:: Parser HiExpr
parserByteArray = do
  _ <- try $ string "[" <* space <* string "#"
  _ <- space
  t <- sepEndBy parserByte space1
  _ <- string "#" <* space <* string "]"
  return $ HiExprValue $ HiValueBytes (BS.pack t)

parserArgs:: Parser HiExpr -> Parser [HiExpr]
parserArgs = parserSequence '(' ')'

parseBlock4 :: HiFun -> Parser HiExpr
parseBlock4 fun = do
  args <- optional (parserArgs parser3)
  let exprFun = (HiExprValue . HiValueFunction) fun in
    return $ case args of
      Nothing    -> exprFun
      Just args' -> HiExprApply exprFun args'

parseOperator :: Parser HiFun
parseOperator = do
  e <- choice $ string <$> listOfAllOperators
  return $ convertToOperation e

parseNumber :: Parser Rational
parseNumber = do
  sign <- optional $ char '-'
  space
  value <- L.scientific
  let scale = if isNothing sign then 1 else -1 in
    return $ toRational value * scale

parseBool :: Parser Bool
parseBool = do
  s <- choice $ string <$> ["false", "true"]
  return $ (== "true") s

parseNull :: Parser HiValue
parseNull = do
  _ <- string "null"
  return HiValueNull

parseString :: Parser HiValue
parseString = do
  let quote = char '"' in do
    _ <- quote
    str <- manyTill L.charLiteral quote
    return $ HiValueString (T.pack str)

parseList :: Parser HiExpr -> Parser [HiExpr]
parseList = parserSequence'
  (oneCharWithoutContinuation' "[" "#")
  (string "]")

parseSpecialAction :: Parser HiAction
parseSpecialAction = do
  s <- choice $ string <$> listOfAllSpecialActions
  return $ convertToAction s

parseKeyValue :: Parser (HiExpr, HiExpr)
parseKeyValue = do
  key <- parser3
  _ <- string ":"
  val <- parser3
  return (key, val)

parseDict :: Parser HiExpr
parseDict = do
  _ <- string "{"
  pairs <- sepBy parseKeyValue (lexeme space (string ","))
  _ <- string "}"
  return $ HiExprDict pairs

parseField :: Parser HiValue
parseField = do
  _ <- string "."
  HiValueString . T.pack <$> ((:) <$> satisfy isAlpha <*>
    many (satisfy (\c -> isAlphaNum c || c == '-')))

parserTerm :: Parser HiExpr
parserTerm = do
  space
  res <- choice [
    HiExprValue . HiValueNumber <$> parseNumber,
    HiExprValue . HiValueBool <$> parseBool,
    HiExprValue <$> parseNull,
    HiExprValue <$> parseString,
    HiExprValue . HiValueFunction <$> parseOperator,
    HiExprApply (HiExprValue (HiValueFunction HiFunList)) <$> parseList parser3,
    HiExprValue . HiValueAction <$> parseSpecialAction,
    parserByteArray,
    parseDict,
    char '(' *> parser3 <* char ')']
  space
  blocks <- many (
    (betweenSpace (Args <$> parserArgs parser3)
    <|>
    (betweenSpace $ Run <$> string "!")
    <|>
    (Field <$> parseField)))
  space
  return
    $ foldl screw res blocks
      where screw = \ac x ->
              case x of
                Args exprs  -> HiExprApply ac exprs
                Run _       -> HiExprRun ac
                Field field -> HiExprApply ac [HiExprValue field]

betweenSpace :: Parser a -> Parser a
betweenSpace p = space *> p <* space

data PostfixElement =
    Args [HiExpr]
  | Run String
  | Field HiValue

parser3 :: Parser HiExpr
parser3 = makeExprParser parserTerm operations

