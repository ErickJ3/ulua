module Parser where

import AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Void
import Control.Monad (void)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

keywords :: [String]
keywords = ["if", "then", "else", "end", "true", "false", "nil", "and", "or", "not"]

keyword :: String -> Parser ()
keyword w = lexeme (string w *> notFollowedBy alphaNumChar)

pIdent :: Parser String
pIdent = (lexeme . try) $ do
  name <- (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
  if name `elem` keywords
    then fail $ "unexpected keyword: " ++ name
    else return name

pNumber :: Parser Value
pNumber = VNumber . fromInteger <$> lexeme L.decimal

pBool :: Parser Value
pBool = (keyword "true" *> pure (VBool True)) <|> (keyword "false" *> pure (VBool False))

pNil :: Parser Value
pNil = keyword "nil" *> pure VNil

pString :: Parser Value
pString = VString <$> (char '"' *> manyTill L.charLiteral (char '"'))

pValue :: Parser Expr
pValue = EValue <$> (pNumber <|> pBool <|> pNil <|> pString)

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable
  where
    pTerm = choice
      [ between (symbol "(") (symbol ")") pExpr
      , pValue
      , EVar <$> pIdent
      ]
    operatorTable =
      [ [ Prefix (EUnOp Not <$ symbol "not") ]
      , [ InfixL (EBinOp Multiply <$ symbol "*")
        , InfixL (EBinOp Divide   <$ symbol "/") ]
      , [ InfixL (EBinOp Add      <$ symbol "+")
        , InfixL (EBinOp Subtract <$ symbol "-") ]
      , [ InfixN (EBinOp Eq  <$ symbol "==")
        , InfixN (EBinOp Neq <$ symbol "~=")
        , InfixN (EBinOp Lt  <$ symbol "<")
        , InfixN (EBinOp Gt  <$ symbol ">")
        , InfixN (EBinOp Lte <$ symbol "<=")
        , InfixN (EBinOp Gte <$ symbol ">=") ]
      , [ InfixR (EBinOp And <$ symbol "and") ]
      , [ InfixR (EBinOp Or  <$ symbol "or") ]
      ]

pStatement :: Parser Statement
pStatement = choice
  [ pIf
  , try pAssign
  , SExpr <$> pExpr
  ]

pAssign :: Parser Statement
pAssign = SAssign <$> pIdent <* symbol "=" <*> pExpr


pIf :: Parser Statement
pIf = SIf
  <$> (keyword "if" *> pExpr)
  <*> (keyword "then" *> manyTill pStatement (lookAhead (void (keyword "else") <|> void (keyword "end"))))
  <*> optional (keyword "else" *> manyTill pStatement (lookAhead (void (keyword "end"))))
  <* keyword "end"

pBlock :: Parser Block
pBlock = many pStatement

parseFile :: String -> IO (Either (ParseErrorBundle String Void) Block)
parseFile filename = do
    input <- readFile filename
    let programParser = sc *> pBlock <* eof
    return $ parse programParser filename input