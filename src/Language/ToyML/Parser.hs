module Language.ToyML.Parser( parseExpr, parseExprFile ) where

import Language.ToyML.Syntax.Parsed

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language(emptyDef)
import Prelude hiding(Ordering(..))

langDef :: P.LanguageDef st
langDef = emptyDef { P.commentStart = "(*"
                   , P.commentEnd   = "*)"
                   , P.reservedNames = ["let", "in", "fun", "true", "false"
                                       ,"if", "then", "else"]
                   , P.reservedOpNames = ["+","-","*","/", "=", "<>", "->"
                                       ,"&&","||", "<", ">", "<=", ">="]
                   }

parseExpr :: String -> Either ParseError Exp
parseExpr = parse (whiteSpace *> expr <* eof) "stdin" 

parseExprFile :: String -> IO (Either ParseError Exp)
parseExprFile = parseFromFile (whiteSpace *> expr <* eof)

lexer :: P.TokenParser st
lexer = P.makeTokenParser langDef 

identifier :: Parser String
identifier = P.identifier lexer

natural :: Parser Integer
natural = P.natural lexer

reserved, reservedOp :: String -> Parser ()
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer


at :: Parser (SourcePos -> a) -> Parser a
at builder = flip ($) <$> getPosition <*> builder 

atom :: Parser Exp
atom = at litBool <|> at litInt <|> at var <|> parens expr
    where
    litBool = mkBool True <$ reserved "true" 
            <|> mkBool False <$ reserved "false" 
    litInt = mkInt <$> natural
    var = mkVar <$> identifier

app :: Parser Exp
app = foldl (\acc v -> mkApp acc v (position v)) <$> atom <*> many atom

expr :: Parser Exp
expr = buildExpressionParser table term 
    where
    table = [ [ prefix "-" (mkPrefix Minus), prefix "+" (mkPrefix Plus) ]
            , [ binary "*" (mkInfix Mul) AssocLeft 
              , binary "/" (mkInfix Div) AssocLeft ]
            , [ binary "+" (mkInfix Add) AssocLeft
              , binary "-" (mkInfix Sub) AssocLeft ] 
            , [ binary "=" (mkInfix EQ) AssocNone
              , binary "<>" (mkInfix NEQ) AssocNone
              , binary "<" (mkInfix LT) AssocNone
              , binary ">" (mkInfix GT) AssocNone
              , binary "<=" (mkInfix LTE) AssocNone
              , binary ">=" (mkInfix GTE) AssocNone ]
            , [ binary "&&" mkAnd AssocLeft ]
            , [ binary "||" mkOr  AssocLeft ] ]
    prefix name f = Prefix (at $ flip f <$ reservedOp name)
    binary name f = Infix (at $ (\p e1 e2 -> f e1 e2 p) <$ reservedOp name) 
    term = at letin <|> at fun <|> at ifE <|> app
    letin = mkLet <$> (reserved "let" *> identifier)
                  <*> (reservedOp "=" *> expr)
                  <*> (reserved "in"  *> expr)
    fun = mkAbs <$> (reserved "fun" *> identifier)
                <*> (reservedOp "->" *> expr)
    ifE = do
        e1 <- reserved "if" *> expr
        e2 <- reserved "then" *> expr
        (reserved "else" *> (mkIfTE e1 e2 <$> expr)) <|> (pure (mkIfT e1 e2))
    
