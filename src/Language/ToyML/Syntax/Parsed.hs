module Language.ToyML.Syntax.Parsed where

import Language.ToyML.Syntax.Base
import Text.Parsec.Pos(SourcePos)
import Text.Parsec.Expr(Assoc(..))
import Text.PrettyPrint.HughesPJClass

data Lit = CInt Integer | CBool Bool
data Op = Op { headChar :: Char, opName :: String }

type instance Ident Exp = String
type instance Literal Exp = Lit
type instance InfixOp Exp = Op
type instance PrefixOp Exp = Op

data Exp where
    Exp :: WellFormed c Exp arg => 
           SConst c -> arg -> SourcePos -> Exp
    
type ExpBuilder = SourcePos -> Exp

position :: Exp -> SourcePos
position (Exp _ _ p) = p

mkVar :: String -> ExpBuilder
mkVar = Exp SVar 

mkInt :: Integer -> ExpBuilder
mkInt = Exp SLiteral . CInt

mkBool :: Bool -> ExpBuilder
mkBool = Exp SLiteral . CBool

mkInfix :: Op -> Exp -> Exp -> ExpBuilder
mkInfix op e1 e2 = Exp SInfix (op, e1, e2)

mkPrefix :: Op -> Exp -> ExpBuilder
mkPrefix = curry $ Exp SPrefix 

mkApp :: Exp -> Exp -> ExpBuilder
mkApp = curry $ Exp SApp

mkAbs :: String -> Exp -> ExpBuilder
mkAbs = curry $ Exp SAbs

mkLet :: String -> Exp -> Exp -> ExpBuilder
mkLet x e1 e2 = Exp SLet (x, e1, e2)

mkIfT :: Exp -> Exp -> ExpBuilder
mkIfT = curry $ Exp SIfT

mkIfTE :: Exp -> Exp -> Exp -> ExpBuilder
mkIfTE e1 e2 e3 = Exp SIfTE (e1, e2, e3)

mkCond :: CondOp -> Exp -> Exp -> ExpBuilder
mkCond condOp e1 e2 = Exp SCond (condOp, e1, e2)

mkAnd, mkOr :: Exp -> Exp -> ExpBuilder
mkAnd e1 e2 = Exp SCond (And, e1 , e2)
mkOr  e1 e2 = Exp SCond (Or , e1 , e2)

infixInfo :: Char -> (Rational, Assoc)
infixInfo ch = case ch of
    '*' -> (7, AssocLeft)
    '/' -> (7, AssocLeft)
    '+' -> (6, AssocLeft)
    '-' -> (6, AssocLeft)
    '<' -> (4, AssocNone)
    '=' -> (4, AssocNone)
    '>' -> (4, AssocNone)
    _   -> infixDefault 

infixDefault :: (Rational, Assoc)
infixDefault = (7, AssocLeft)

prefixInfo :: Char -> Rational
prefixInfo ch = case ch of
    '+' -> 8
    '-' -> 8
    _   -> 8

condInfo :: CondOp -> (Rational, String, Assoc)
condInfo And = (3, "&&", AssocLeft)
condInfo Or  = (2, "||", AssocLeft)

instance Pretty Exp where
    pPrintPrec l prec (Exp label arg _) = prettyParen (dPrec < prec) doc
        where
        binary :: Doc -> Rational -> Assoc -> Exp -> Exp -> (Rational, Doc)
        binary name opPrec assoc e1 e2 = (opPrec, d1 <+> name <+> d2)
            where
            (lPrec,rPrec) = case assoc of
                AssocLeft -> (opPrec, opPrec + 1)
                AssocNone -> (opPrec + 1, opPrec + 1)
                AssocRight -> (opPrec + 1, opPrec)
            d1 = pPrintPrec l lPrec e1
            d2 = pPrintPrec l rPrec e2
        (dPrec, doc) = case (label, arg) of
            (SVar, x) -> (10, text x)
            (SLiteral, CInt i) -> (if i >= 0 then 10 else 9, integer i)
            (SLiteral, CBool b) -> (10, text $ if b then "true" else "false")
            (SInfix, (op,e1,e2)) -> binary (text (opName op)) opPrec assoc e1 e2
                where
                (opPrec, assoc) = infixInfo (headChar op)
            (SPrefix, (op,e)) -> (opPrec, text (opName op) <+> pPrintPrec l (opPrec + 1) e)
                where opPrec = prefixInfo (headChar op)
            (SApp, (e1, e2)) -> binary empty 9 AssocLeft e1 e2
            (SAbs, (x, e)) -> 
                (8.5, text "fun" <+> text x <+> text "->" $+$ 
                        nest 4 (pPrintPrec l 0 e))
            (SLet, (x, e1, e2)) ->
                (8.5, text "let" <+> text x <+> text "=" <+> 
                        pPrintPrec l 0 e1 <+> text "in" $+$
                        pPrintPrec l 0 e2)
            (SIfT, (e1,e2)) ->
                (8.5, text "if" <+> pPrintPrec l 0 e1 $+$
                      text "then" <+> pPrintPrec l 0 e2)
            (SIfTE, (e1, e2, e3)) ->
                (8.5, text "if" <+> pPrintPrec l 0 e1 $+$
                      text "then" <+> pPrintPrec l 0 e2 $+$
                      text "else" <+> pPrintPrec l 0 e3)
            (SCond, (cond, e1, e2)) -> binary (text name) opPrec assoc e1 e2
                where
                (opPrec, name, assoc) = condInfo cond

