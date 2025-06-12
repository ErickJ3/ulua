module Evaluator where

import AST
import Control.Monad (void) 
import Control.Monad.State
import Control.Monad.Except
import Data.Map (Map, (!?), insert, empty)

data EvalError = VarNotFound String
               | TypeMismatch String
               | DivisionByZero
               deriving (Show)

type Eval a = StateT Environment (ExceptT EvalError IO) a

runEval :: Eval a -> IO (Either EvalError (a, Environment))
runEval eval = runExceptT (runStateT eval empty)

evalBlock :: Block -> Eval ()
evalBlock = mapM_ evalStatement

evalStatement :: Statement -> Eval ()
evalStatement (SAssign var expr) = do
  val <- evalExpr expr
  modify (insert var val) 

evalStatement (SIf cond thenBlock maybeElseBlock) = do
  condVal <- evalExpr cond
  case condVal of
    VBool isTrue -> if isTrue
      then evalBlock thenBlock
      else case maybeElseBlock of
        Just elseBlock -> evalBlock elseBlock
        Nothing        -> return ()
    _ -> throwError $ TypeMismatch "The 'if' condition must be a boolean."

evalStatement (SExpr expr) = void (evalExpr expr)

evalExpr :: Expr -> Eval Value
evalExpr (EValue val) = return val
evalExpr (EVar var) = do
  env <- get
  maybe (throwError $ VarNotFound var) return (env !? var)

evalExpr (EUnOp op expr) = do
  val <- evalExpr expr
  applyUOp op val

evalExpr (EBinOp And e1 e2) = do
    v1 <- evalExpr e1
    case v1 of
        VBool False -> return (VBool False)
        VBool True  -> evalExpr e2
        _           -> throwError $ TypeMismatch "'and' operator requires booleans."

evalExpr (EBinOp Or e1 e2) = do
    v1 <- evalExpr e1
    case v1 of
        VBool True -> return (VBool True) 
        VBool False  -> evalExpr e2
        _           -> throwError $ TypeMismatch "'or' operator requires booleans."

evalExpr (EBinOp op e1 e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  applyBinOp op v1 v2


applyUOp :: UOp -> Value -> Eval Value
applyUOp Not (VBool b) = return $ VBool (not b)
applyUOp Not _         = throwError $ TypeMismatch "Operator 'not' requires a boolean."

applyBinOp :: Op -> Value -> Value -> Eval Value
applyBinOp Add (VNumber n1) (VNumber n2) = return $ VNumber (n1 + n2)
applyBinOp Subtract (VNumber n1) (VNumber n2) = return $ VNumber (n1 - n2)
applyBinOp Multiply (VNumber n1) (VNumber n2) = return $ VNumber (n1 * n2)
applyBinOp Divide (VNumber n1) (VNumber n2)
    | n2 == 0   = throwError DivisionByZero
    | otherwise = return $ VNumber (n1 / n2)
applyBinOp Eq v1 v2 = return $ VBool (v1 == v2)
applyBinOp Neq v1 v2 = return $ VBool (v1 /= v2)
applyBinOp Lt (VNumber n1) (VNumber n2) = return $ VBool (n1 < n2)
applyBinOp Gt (VNumber n1) (VNumber n2) = return $ VBool (n1 > n2)
applyBinOp Lte (VNumber n1) (VNumber n2) = return $ VBool (n1 <= n2)
applyBinOp Gte (VNumber n1) (VNumber n2) = return $ VBool (n1 >= n2)
applyBinOp op _ _ = throwError $ TypeMismatch ("Operator " ++ show op ++ " applied to invalid types.")
