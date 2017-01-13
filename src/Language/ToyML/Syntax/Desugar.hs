module Language.ToyML.Syntax.Desugar where

import Language.ToyML.Syntax.Base
import qualified Language.ToyML.Syntax.Parsed as E
import Language.ToyML.Syntax.Parsed(Lit(..))
import Text.Parsec.Pos(SourcePos)
import Data.Void(absurd)
import Data.Kind(Constraint)
import Text.PrettyPrint.HughesPJClass
import Text.Parsec.Expr(Assoc(..))

data Exp where
    Exp :: (WellFormed c Exp arg, Desugared c Exp arg) => 
           SConst c -> arg -> SourcePos -> Exp

type instance Ident Exp = String
type instance Literal Exp = Lit

type family Desugared (c :: Const) e arg :: Constraint where
    Desugared 'Infix e arg = Impossible 'Infix
    Desugared 'Prefix e arg = Impossible 'Prefix
    Desugared 'IfT e arg = Impossible 'IfT
    Desugared 'Cond e arg = Impossible 'Cond
    Desugared c e arg = ()

type ExpBuilder = SourcePos -> Exp

position :: Exp -> SourcePos
position (Exp _ _ p) = p

mkVar :: String -> ExpBuilder
mkVar = Exp SVar 

mkLiteral :: E.Lit -> ExpBuilder
mkLiteral = Exp SLiteral 

mkInt :: Integer -> ExpBuilder
mkInt = Exp SLiteral . CInt

mkBool :: Bool -> ExpBuilder
mkBool = Exp SLiteral . CBool

mkApp :: Exp -> Exp -> ExpBuilder
mkApp = curry $ Exp SApp

mkAbs :: String -> Exp -> ExpBuilder
mkAbs = curry $ Exp SAbs

mkLet :: String -> Exp -> Exp -> ExpBuilder
mkLet x e1 e2 = Exp SLet (x, e1, e2)

mkIfTE :: Exp -> Exp -> Exp -> ExpBuilder
mkIfTE e1 e2 e3 = Exp SIfTE (e1, e2, e3)

desugar :: E.Exp -> Exp
desugar (E.Exp label arg pos) = 
    case (label,arg) of
        (SVar, x) -> mkVar x pos
        (SLiteral, l) -> mkLiteral l pos
        (SInfix, (op, e1, e2)) -> 
            let e1' = desugar e1
                e2' = desugar e2 
            in mkApp (mkApp (mkVar (E.opName op) pos) e1' pos) e2' pos
        (SPrefix, (op, e)) -> 
            let e' = desugar e in
            mkApp (mkVar (E.opName op) pos) e' pos
        (SApp, (e1,e2)) -> mkApp (desugar e1) (desugar e2) pos
        (SAbs, (x, e)) -> mkAbs x (desugar e) pos
        (SLet, (x, e1, e2)) -> mkLet x (desugar e1) (desugar e2) pos
        (SIfT, (e1, e2)) -> mkIfTE (desugar e1) (desugar e2) (mkInt 0 pos) pos
        (SIfTE, (e1, e2, e3)) -> mkIfTE (desugar e1) (desugar e2) (desugar e3) pos
        (SCond, (And, e1, e2)) -> mkIfTE (desugar e1) (desugar e2) (mkBool False pos) pos
        (SCond, (Or, e1, e2)) -> mkIfTE (desugar e1) (mkBool True pos) (desugar e2) pos

instance Pretty Exp where
    pPrintPrec l prec (Exp label arg _) = prettyParen (dPrec < prec) doc
        where
        binary :: Rational -> Assoc -> Exp -> Exp -> (Rational, Doc)
        binary opPrec assoc e1 e2 = (opPrec, d1 <+> d2)
            where
            (lPrec,rPrec) = case assoc of
                AssocLeft -> (opPrec, opPrec + 1)
                AssocNone -> (opPrec + 1, opPrec + 1)
                AssocRight -> (opPrec + 1, opPrec)
            d1 = pPrintPrec l lPrec e1
            d2 = pPrintPrec l rPrec e2
        (dPrec, doc) = case (label, arg) of
            (SVar, x) -> (10, text x)
            (SLiteral, E.CInt i) -> (if i >= 0 then 10 else 9, integer i)
            (SLiteral, E.CBool b) -> (10, text $ if b then "true" else "false")
            (SInfix, _)  -> absurd (impossible label)
            (SPrefix, _) -> absurd (impossible label)
            (SApp, (e1, e2)) -> binary 9 AssocLeft e1 e2
            (SAbs, (x, e)) -> 
                (8.5, text "fun" <+> text x <+> text "->" $+$ 
                        nest 4 (pPrintPrec l 0 e))
            (SLet, (x, e1, e2)) ->
                (8.5, text "let" <+> text x <+> text "=" <+> 
                        pPrintPrec l 0 e1 <+> text "in" $+$
                        pPrintPrec l 0 e2)
            (SIfT, _) -> absurd (impossible label)
            (SIfTE, (e1, e2, e3)) ->
                (8.5, text "if" <+> pPrintPrec l 0 e1 $+$
                      text "then" <+> pPrintPrec l 0 e2 $+$
                      text "else" <+> pPrintPrec l 0 e3)
            (SCond, _) -> absurd (impossible label)

