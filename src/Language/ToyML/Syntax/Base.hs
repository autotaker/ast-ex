module Language.ToyML.Syntax.Base where
import Data.Void(Void)
import Data.Kind(Constraint)

data Const = Var | Literal | Infix | Prefix 
           | App | Abs | Let  
           | IfT | IfTE
           | Cond

data SConst (c :: Const) where
    SVar     :: SConst 'Var
    SLiteral :: SConst 'Literal
    SInfix   :: SConst 'Infix
    SPrefix  :: SConst 'Prefix
    SApp     :: SConst 'App
    SAbs     :: SConst 'Abs
    SLet     :: SConst 'Let
    SIfT     :: SConst 'IfT
    SIfTE    :: SConst 'IfTE
    SCond    :: SConst 'Cond

type family Ident e
type family Literal e
type family InfixOp e
type family PrefixOp e

data CondOp = And | Or

type family WellFormed (c :: Const) e arg :: Constraint where
    WellFormed 'Var e arg     = arg ~ Ident e
    WellFormed 'Literal e arg = arg ~ Literal e
    WellFormed 'Infix e arg   = arg ~ (InfixOp e,e,e)
    WellFormed 'Prefix e arg  = arg ~ (PrefixOp e, e)
    WellFormed 'App e arg     = arg ~ (e,e)
    WellFormed 'Abs e arg     = arg ~ (Ident e, e)
    WellFormed 'Let e arg     = arg ~ (Ident e, e, e)
    WellFormed 'IfT e arg     = arg ~ (e, e)
    WellFormed 'IfTE e arg    = arg ~ (e, e, e)
    WellFormed 'Cond e arg    = arg ~ (CondOp, e, e)
  
class Impossible (c :: Const) where
    impossible :: SConst c -> Void
