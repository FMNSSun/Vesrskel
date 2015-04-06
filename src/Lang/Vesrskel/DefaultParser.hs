

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

import Data.Maybe

data Procedure = Procedure {
   procName :: String,
   procReturnType :: Type,
   procParams :: [Parameter],
   procLocals :: [Local],
   procBody :: [Statement]
 } deriving Show

data Type = Type {
   typeName :: String
 } deriving Show

data Parameter = Parameter {
   paramType :: Type,
   paramName :: String
 } deriving Show

data Local = Local {
   localType :: Type,
   localName :: String
 } deriving Show

data Statement = Assignment String Expression | While Expression [Statement]
 deriving Show

data Expression = None | Var String | BinOp String Expression Expression |
      IntVal Int
 deriving Show


keyword kw = do
  string kw
  optional spaces
  return kw

keyword' kw = do
  k <- oneOf kw
  optional spaces
  return [k]

parseProcedure :: Parser Procedure
parseProcedure = do
  keyword "procedure"
  t <- parseType
  n <- parseName
  keyword "params"
  params <- many $ parseParameter
  keyword "locals"
  locals <- many $ parseLocal
  keyword "begin"
  st <- many $ parseStatement
  keyword "end"
  return $ Procedure {
      procName = n,
      procReturnType = t,
      procParams = params,
      procLocals = locals,
      procBody = st
    }

parseParameter :: Parser Parameter
parseParameter = do
  t <- parseType
  name <- parseName
  return $ Parameter { paramType = t, paramName = name }

parseLocal :: Parser Local
parseLocal = do
  t <- parseType
  name <- parseName
  return $ Local { localType = t, localName = name }

parseType :: Parser Type
parseType = do
  arr <- many $ oneOf "@"
  name' <- oneOf ['A'..'Z']
  name <- many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z']
  optional spaces
  return $ Type { typeName = (arr ++ [name'] ++ name) }

parseName :: Parser String
parseName = do
  name' <- oneOf $ ['A'..'Z']
  name <- many $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
  optional spaces
  return (name' : name)

parseStatement :: Parser Statement
parseStatement = do
  parseAssignment <|> parseWhile

parseExpression :: Parser Expression
parseExpression = do
  parseVar <|> parseBinOp <|> parseIntVal

parseIntVal :: Parser Expression
parseIntVal = do
  n <- optionMaybe $ char '-'
  dg <- many1 $ oneOf ['0'..'9']
  let sign = fromMaybe '0' n
  optional spaces
  return $ IntVal (read $ sign : dg)

parseVar :: Parser Expression
parseVar = do
  n <- parseName
  return $ Var n

parseBinOp :: Parser Expression
parseBinOp = do
  keyword "("
  e1 <- parseExpression
  op <- (try $ keyword ">=") <|> (try $ keyword "<=") <|> (keyword' "+-/*")
  e2 <- parseExpression
  keyword ")"
  return $ BinOp op e1 e2

parseAssignment :: Parser Statement
parseAssignment = do
  n <- parseName
  keyword ":="
  exp <- parseExpression
  return $ Assignment n exp

parseWhile :: Parser Statement
parseWhile = do
  keyword "while"
  exp <- parseExpression
  keyword "do"
  st <- many $ parseStatement
  keyword "end"
  return $ While exp st

runParserWithString p input = 
  case parse p "" input of
    Left err -> error $ show err
    Right q -> q

parseFile path = do
  txt <- readFile path
  return $ runParserWithString parseProcedure txt
