module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Prim hiding (try)
import System.Environment
import Control.Monad
import Control.Monad.Except
import Data.List
import Data.Char
import Data.Maybe


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_"


spaces :: Parser ()
spaces = skipMany1 space

-- readExpr :: String -> String
-- readExpr input = case parse (spaces >> symbol) "lisp" input of
--   Left err -> "No match: " ++ show err
--   Right val -> "Found value: " ++ show val

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             deriving (Eq)

instance Show LispVal where show = showVal


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError

type ThrowsError = Either LispError

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many singleChar
  _ <- char '"'
  return $ String x

  where
    unescapedChar :: Parser Char
    unescapedChar = noneOf "\\\""
    escapedChar :: Parser Char
    escapedChar = char '\\' >> oneOf "\"\n\r\t\\"
    singleChar :: Parser Char
    singleChar = unescapedChar <|> escapedChar


parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  case first of
    '#' -> parseHashLiteral
    _ -> do
      rest <- many (letter <|> digit <|> symbol)
      let atom = first:rest
      return $ Atom atom


parseHashLiteral :: Parser LispVal
parseHashLiteral = do
  litType <- oneOf "tfbodx\\"
  case litType of
    't' -> return (Bool True)
    'f' -> return (Bool False)
    'b' -> parseBinaryNumber
    'o' -> parseOctNumber
    'd' -> parseNumber
    'x' -> parseHexNumber
    '\\' -> parseCharacterLiteral
    _ -> parserFail "unrecognized hash literal"

newtype NumberBase = NumberBase { unBase :: Int } deriving (Show, Eq)

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit


parseOctNumber :: Parser LispVal
parseOctNumber = Number . fromJust . numberConverter (NumberBase 8) <$> many1 octDigit


parseHexNumber :: Parser LispVal
parseHexNumber = Number . fromJust . numberConverter (NumberBase 16) <$> many1 hexDigit


parseBinaryNumber :: Parser LispVal
parseBinaryNumber = Number . readBinaryNumber <$> many1 binaryDigit
  where
    readBinaryNumber :: String -> Integer
    readBinaryNumber = fromJust . numberConverter (NumberBase 2)
    binaryDigit :: Parser Char
    binaryDigit = oneOf "01"


numberConverter :: NumberBase -> String -> Maybe Integer
numberConverter base = foldM (foldingFunction (unBase base)) 0
  where
    foldingFunction :: Int -> Integer -> Char -> Maybe Integer
    foldingFunction b acc d = do
      val <- digitValue b d
      return $ acc * toInteger b + toInteger val
    digitValue :: Int -> Char -> Maybe Int
    digitValue b d =
      elemIndex (toLower d) "0123456789abcdef"
      >>= cutoffToBase b
    cutoffToBase :: Int -> Int -> Maybe Int
    cutoffToBase b v = if v < b then return v else Nothing


parseCharacterLiteral :: Parser LispVal
parseCharacterLiteral = do
  first <- noneOf " "
  rest <- many $ noneOf " "
  case first:rest of
    [c] -> return $ Character c
    "space" -> return $ Character ' '
    "newline" -> return $ Character '\n'
    _ -> parserFail "unknown character literal"


parseList :: Parser LispVal
parseList =  List <$> sepBy parseExpr space


parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t


parseParensList :: Parser LispVal
parseParensList =  do
  _ <- char '('
  x <- try parseList <|> parseDottedList
  _ <- char ')'
  return x


parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]


parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
            <|> parseQuoted
            <|> parseParensList


readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right x -> x

showVal :: LispVal -> String
showVal (Character c) = ['\'', c, '\'']
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head' tail') = "(" ++ unwordsList head' ++ " . " ++ showVal tail' ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                          ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                          ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr


eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args


apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives


primitives :: [ (String, [LispVal] -> LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)
             ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params


unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
  if null parsed
  then 0
  else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0


trapError action = catchEror action (return . show)


extractValue :: ThrowsError a -> a
extractValue (Right val) = val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
