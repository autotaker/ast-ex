{-# LANGUAGE RankNTypes #-}
module Language.ToyML.Eval(evalTop) where

import Language.ToyML.Syntax.Desugar
import Language.ToyML.Syntax.Parsed(Lit(..))
import qualified Data.Map as M
import Control.Monad.Except
import Text.Parsec.Pos(SourcePos)
import Language.ToyML.Syntax.Base
import Data.Void(absurd)

data Value = VInt Integer
           | VBool Bool
           | VClose Env String (Either Exp PExp)

type PExp = Env -> Except String Value

instance Show Value where
    show (VInt i) = show i
    show (VBool b) = if b then "true" else "false"
    show (VClose _ _ _) = "<fun>"

type Env = M.Map String Value

data EvalError = EvalError (SourcePos, String)

instance Show EvalError where
    show (EvalError (p, msg)) = "EvalError :" ++ msg ++ "\nat " ++ show p

intOp :: String -> (Integer -> Integer -> Integer) -> Value
intOp name f = 
    VClose M.empty "x" $ Right $ \env1 -> 
        return $ VClose env1 "y" $ Right $ \env -> 
            case (env M.! "x", env M.! "y") of
                (VInt x, VInt y) -> return (VInt (f x y))
                (v1, v2) -> throwError $ 
                    "Invalid argument for operation " ++ name ++ ":" ++ show (v1, v2)

cmpOp :: String -> (forall a. Ord a => a -> a -> Bool) -> Value
cmpOp name f = 
    VClose M.empty "x" $ Right $ \env1 -> 
        return $ VClose env1 "y" $ Right $ \env -> 
            case (env M.! "x", env M.! "y") of
                (VInt x, VInt y) -> return (VBool (f x y))
                (VBool x, VBool y) -> return (VBool (f x y))
                (v1, v2) -> throwError $ 
                    "Invalid argument for operation " ++ name ++ ":" ++ show (v1, v2)

primitives :: Env
primitives = M.fromList [ ("+", intOp "+" (+)) 
                        , ("-", intOp "-" (-))
                        , ("*", intOp "*" (*))
                        , ("/", intOp "/" div)
                        , ("<", cmpOp "<" (<))
                        , (">", cmpOp ">" (>))
                        , ("=", cmpOp "=" (==))
                        , ("<>", cmpOp "=" (/=))
                        , ("<=", cmpOp "<=" (<=))
                        , (">=", cmpOp ">=" (>=)) ]

evalTop :: Exp -> Either EvalError Value
evalTop e = runExcept (eval primitives e)

eval :: Env -> Exp -> Except EvalError Value
eval env (Exp label arg pos) = 
    case (label, arg) of
        (SVar, x) -> case M.lookup x env of
            Just v -> return v
            Nothing -> throwError $ EvalError(pos, "unbound variable: " ++ x)
        (SLiteral, CInt i) -> return (VInt i)
        (SLiteral, CBool b) -> return (VBool b)
        (SApp, (e1, e2)) -> do
            v1 <- eval env e1
            case v1 of
                VClose env' x me -> do
                    v2 <- eval env e2
                    case me of
                        Left e0 -> eval (M.insert x v2 env') e0
                        Right e0 -> withExcept (\s -> EvalError(pos,s)) $ e0 (M.insert x v2 env')
                _ -> throwError $ EvalError(pos, "a function is expected, but found " ++ show v1)
        (SAbs, (x, e)) -> return $ VClose env x (Left e)
        (SLet, (x, e1, e2)) -> do
            v1 <- eval env e1
            eval (M.insert x v1 env) e2
        (SIfTE, (e1, e2, e3)) -> do
            v1 <- eval env e1
            case v1 of
                VBool True -> eval env e2
                VBool False -> eval env e3
                _ -> throwError $ EvalError(pos, "a boolean value is expected, but found " ++ show v1)
        (SInfix, _) -> absurd (impossible label)
        (SPrefix, _) -> absurd (impossible label)
        (SIfT, _) -> absurd (impossible label)
        (SCond, _) -> absurd (impossible label)
