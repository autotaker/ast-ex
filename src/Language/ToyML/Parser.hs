module Language.ToyML.Parser( parseExpr, parseExprFile ) where

import Language.ToyML.Syntax.Parsed
import Language.ToyML.Syntax.Base(CondOp(..))

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language(emptyDef)
import Prelude hiding(Ordering(..))
import Data.List
import Data.Function

langDef :: P.LanguageDef st
langDef = emptyDef { P.commentStart = "(*"
                   , P.commentEnd   = "*)"
                   , P.reservedNames = ["let", "in", "fun", "true", "false"
                                       ,"if", "then", "else"]
                   , P.reservedOpNames = ["->","&&","||"]
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

symbol :: String -> Parser String
symbol = P.symbol lexer

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

prefixLetter, infixLetter :: String
prefixLetter = "+-"
infixLetter = "*/+-<=>"

expr :: Parser Exp
expr = buildExpressionParser table term 
    where
    mkInfix' op p e1 e2 = mkInfix op e1 e2 p
    mkPrefix' op p e1 = mkPrefix op e1 p
    mkCond' cond p e1 e2 = mkCond cond e1 e2 p
    table = map (map snd) $ groupBy ((==) `on` fst) $ reverse $ sortBy (compare `on` fst) $ 
            [ (prec, Prefix (at $ mkPrefix' <$> parseOp ch)) 
                | ch <- prefixLetter, let prec = prefixInfo ch ] ++  -- prefix operators
            [ (prec, Infix (at $ mkInfix' <$> parseOp ch) assoc)     -- infix  operators
                | ch <- infixLetter, let (prec, assoc) = infixInfo ch ] ++
            [ (prec, Infix (at $ mkInfix' <$> parseOther) assoc)     -- infix other operators
                | let (prec, assoc) = infixDefault ] ++
            [ (prec, Infix (at $ (mkCond' cond) <$ reservedOp name) assoc)  -- conditional operators
                | cond <- [And, Or], let (prec, name, assoc) = condInfo cond ]
    parseOp ch = P.lexeme lexer $ try $ do
        _ <- char ch
        cs <- many (P.opLetter langDef)
        let name = ch : cs
        if name `elem` P.reservedOpNames langDef
            then unexpected ("reserved operator " ++ show name)
            else return (Op { headChar = ch, opName = ch:cs })
    parseOther = P.lexeme lexer $ try $ do
        ch <- oneOf infixLetter
        cs <- many (P.opLetter langDef)
        let name = ch : cs
        if name `elem` P.reservedOpNames langDef 
            then unexpected ("reserved operator " ++ show name)
            else return (Op { headChar = ch, opName = name })
    term = at letin <|> at fun <|> at ifE <|> app
    letin = mkLet <$> (reserved "let" *> identifier)
                  <*> (symbol "=" *> expr)
                  <*> (reserved "in"  *> expr)
    fun = mkAbs <$> (reserved "fun" *> identifier)
                <*> (reservedOp "->" *> expr)
    ifE = do
        e1 <- reserved "if" *> expr
        e2 <- reserved "then" *> expr
        (reserved "else" *> (mkIfTE e1 e2 <$> expr)) <|> (pure (mkIfT e1 e2))
    
