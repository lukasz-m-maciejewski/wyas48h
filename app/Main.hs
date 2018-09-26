{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Prim hiding (try)
import System.Environment
import Control.Monad
import Control.Monad.Except
import Data.List
import Data.Char
import Data.Maybe
import Data.IORef
import System.IO


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_"


spaces :: Parser ()
spaces = skipMany1 space


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String]
                    , vararg :: Maybe String
                    , body :: [LispVal]
                    , closure :: Env
                    }

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


type Env = IORef [(String, IORef LispVal)]


type IOThrowsError = ExceptT LispError IO


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
    escapedChar = char '\\' >> oneOf "\"\\nrta"
    singleChar :: Parser Char
    singleChar = unescapedChar <|> (transformChar <$> escapedChar)
    transformChar :: Char -> Char
    transformChar c = case c of
      'n' -> '\n'
      't' -> '\t'
      'r' -> '\r'
      'a' -> '\a'
      anyOther -> anyOther


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


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right x -> return x

showVal :: LispVal -> String
showVal (Character c) = ['\'', c, '\'']
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head' tail') = "(" ++ unwordsList head' ++ " . " ++ showVal tail' ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal Func {params = args, vararg = varargs, body = _, closure = _} =
  "(lambda (" ++ unwords (map show args) ++
  (case varargs of
     Nothing -> ""
     Just arg -> " . " ++ arg)
  ++ ") ...)"

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
showError _ = error "invalid code path"


eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval env (Atom identifier) = getVar env identifier
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", predicate, conseq, alt]) = do
  predicateValue <- eval env predicate
  case predicateValue of
    Bool False -> eval env alt
    _ -> eval env conseq

eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define": List (Atom var : params') : body')) =
  makeNormalFunc env params' body' >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params') varargs : body')) =
  makeVarArgs varargs env params' body' >>= defineVar env var
eval env (List (Atom "lambda" : List params' : body')) =
  makeNormalFunc env params' body'
eval env (List (Atom "lambda" : DottedList params' varargs : body')) =
  makeVarArgs varargs env params' body'
eval env (List (Atom "lambda" : varargs@(Atom _) : body')) =
  makeVarArgs varargs env [] body'
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
--eval env (List (Atom func : args)) = mapM (eval env) args >>= apply func
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params' varargs body' closure') args =
  if num params' /= num args && isNothing varargs
     then throwError $ NumArgs (num params') args
          else liftIO (bindVars closure' $ zip params' args) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params') args
        num = toInteger . length
        evalBody env = last <$> mapM (eval env) body'
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
          Nothing -> return env
apply _ _ = throwError $ NotFunction "Element at head is not a function" ""


makeFunc varargs env params' body' = return $ Func (map showVal params') varargs body' env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal


primitives :: [ (String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)
             , ("=", numBoolBinop (==))
             , ("<", numBoolBinop (>))
             , (">", numBoolBinop (<))
             , ("/=", numBoolBinop (/=))
             , (">=", numBoolBinop (>=))
             , ("<=", numBoolBinop (<=))
             , ("&&", boolBoolBinop (&&))
             , ("||", boolBoolBinop (||))
             , ("string=?", strBoolBinop (==))
             , ("string<?", strBoolBinop (<))
             , ("string>?", strBoolBinop (>))
             , ("string<=?", strBoolBinop (<=))
             , ("string>=?", strBoolBinop (>=))
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             , ("eq?", eqv)
             , ("eqv?", eqv)
             , ("equal?", equal)
             ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params' = Number . foldl1 op <$> mapM unpackNum params'


boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool


unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                         if null parsed
                         then throwError $ TypeMismatch "number" $ String n
                         else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum


unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number n) = return (show n)
unpackStr (Bool b) = return (show b)
unpackStr notString = throwError (TypeMismatch "string" notString)


unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool


trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)


extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _ = error "invalid code path"


car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList


cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList


cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList


eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] = return $ Bool $
                                 (length arg1 == length arg2)
                                 && and (zipWith eqvPair arg1 arg2)
  where eqvPair x1 x2 = case eqv [x1, x2] of
          Left _ -> False
          Right (Bool val) -> val
          _ -> error "code path should be unreachable"
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList


data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)


unpackers :: [Unpacker]
unpackers = [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     catchError (return $ unpacked1 == unpacked2) (const $ return False)


equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2) unpackers
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout


readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine


evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn


evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  if predicate result
    then return ()
    else action result >> until_ predicate prompt action


runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint


runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr


nullEnv :: IO Env
nullEnv = newIORef []


liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val


runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)


isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef


getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . (`writeIORef` value))
    (lookup var env)
  return value


defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value


bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings' env = fmap (++env) (mapM addBinding bindings')
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)


primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
  where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)


main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne (head args)
    _ -> putStrLn "Program takes only 0 or 1 argument"
