module Language.ToyML.Syntax.Parsed where

import Language.ToyML.Syntax.Base
import Text.Parsec.Pos(SourcePos)
import Text.Parsec.Expr(Assoc(..))
import Text.PrettyPrint.HughesPJClass
import Prelude hiding(Ordering(..))

data Lit = CInt Integer | CBool Bool
data IOp = Add | Sub | Mul | Div 
          | LT | GT | LTE | GTE | EQ | NEQ
data POp = Plus | Minus

type instance Ident Exp = String
type instance Literal Exp = Lit
type instance InfixOp Exp = IOp
type instance PrefixOp Exp = POp

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

mkInfix :: IOp -> Exp -> Exp -> ExpBuilder
mkInfix op e1 e2 = Exp SInfix (op, e1, e2)

mkPrefix :: POp -> Exp -> ExpBuilder
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

mkAnd, mkOr :: Exp -> Exp -> ExpBuilder
mkAnd e1 e2 = Exp SCond (And, e1 , e2)
mkOr  e1 e2 = Exp SCond (Or , e1 , e2)

opInfo :: IOp -> (Rational, String, Assoc)
opInfo Mul = (7, "*", AssocLeft)
opInfo Div = (7, "/", AssocLeft)
opInfo Add = (6, "+", AssocLeft)
opInfo Sub = (6, "-", AssocLeft)
opInfo LT  = (4, "<", AssocNone)
opInfo EQ  = (4, "=", AssocNone)
opInfo GT  = (4, ">", AssocNone)
opInfo LTE = (4, "<=", AssocNone)
opInfo GTE = (4, ">=", AssocNone)
opInfo NEQ = (4, "<>", AssocNone)


instance Pretty Exp where
    pPrintPrec l prec (Exp label arg _) = prettyParen (dPrec < prec) doc
        where
        (dPrec, doc) = case (label, arg) of
            (SVar, x) -> (10, text x)
            (SLiteral, CInt i) -> (if i >= 0 then 10 else 9, integer i)
            (SLiteral, CBool b) -> (10, text $ if b then "true" else "false")
            (SInfix, (op,e1,e2)) -> (opPrec, d1 <+> text opName <+> d2)
                where
                (opPrec, opName, assoc) = opInfo op
                (lPrec,rPrec) = case assoc of
                    AssocLeft -> (opPrec, opPrec + 1)
                    AssocNone -> (opPrec + 1, opPrec + 1)
                    AssocRight -> (opPrec + 1, opPrec)
                d1 = pPrintPrec l lPrec e1
                d2 = pPrintPrec l rPrec e2
            (SPrefix, (op,e)) -> (8, text opName <+> pPrintPrec l 9 e)
                where opName = case op of
                            Plus -> "+"
                            Minus -> "-"
            (SApp, (e1, e2)) -> (9, d1 <+> d2)
                where
                d1 = pPrintPrec l 9 e1
                d2 = pPrintPrec l 10 e2
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
            (SCond, (And, e1, e2)) ->
                (3, pPrintPrec l 3 e1 <+> text "&&" <+> pPrintPrec l 4 e2)
            (SCond, (Or, e1, e2)) ->
                (2, pPrintPrec l 2 e1 <+> text "||" <+> pPrintPrec l 3 e2)

