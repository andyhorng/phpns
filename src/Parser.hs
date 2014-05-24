module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Control.Monad
import Data.List

-- PHP Parser
-- A statment is a logical unit like if, while, function...
-- A log of statments can build a statment
-- code consist of many statment
data Code = Code Statement deriving Show

data Statement = Expression String
               | Seq [Statement]
               | Namespace String
               | Use String
               | Class String Statement
               | Function String Statement
               | IF Statement deriving Show

-- Parsec's helper which can build a basic token parser
langDef = emptyDef { Token.commentStart = "/*"
                   , Token.commentEnd = "*/"
                   , Token.commentLine = "//"
                   , Token.identStart = letter
                   , Token.identLetter = alphaNum <|> char '_'
                   , Token.reservedNames = [ "if", "else", "elseif", "while", "break", "do", "for", "continue"
                                           , "true", "false", "null", "and", "or", "class", "function", "return"
                                           , "<?php", "?>", "echo", "print", "namespace", "use"
                                           ]
                   , Token.reservedOpNames = [ "=", "==", "===", "->", ".", "+", "-", "*", "/", "%", "<", ">"
                                             , "and", "or", "||", "&&", "!", "++", "--"
                                             ]
                   }

lexer = Token.makeTokenParser langDef
identifier = Token.identifier lexer
reserved = Token.reserved lexer
parens = Token.parens lexer
braces = Token.braces lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

-- namespace
parsePath :: Parser String
parsePath = do
    path <- sepBy1 identifier $ string "\\"
    return $ intercalate "\\" path

parseNamespace :: Parser Statement
parseNamespace = do
    reserved "namespace"
    path <- parsePath
    semi
    return $ Namespace path

-- use
parseUse :: Parser Statement
parseUse = do
    reserved "use"
    path <- parsePath
    semi
    return $ Use path

-- class
parseClass :: Parser Statement
parseClass = do
    reserved "class"
    name <- identifier
    s <- braces $ many parseStatement
    return $ Class name $ Seq s

parseStatement :: Parser Statement
parseStatement =  parseNamespace
              <|> parseUse
              <|> parseClass

parseCode :: Parser Code
parseCode = do
    reserved "<?php"
    s <- many parseStatement
    return $ Code $ Seq s

parseString :: String -> Either ParseError Code
parseString = parse parseCode "PHP"
