{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Evaluator where

import           Structure

import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Data.Map.Lazy as M

data EvalError = DivizionByZero
               | NoVarBindingFound Ref
    deriving (Eq, Show)

type MonadEvaluator m = (MonadReader Dictionary m, MonadError EvalError m)

newtype Evaluator a = Evaluator { runEvaluator :: (ReaderT Dictionary (Either EvalError)) a }
    deriving (Functor, Applicative, Monad, MonadError EvalError, MonadReader Dictionary)

exprEvaluation :: MonadEvaluator m => Expr -> m Int
exprEvaluation (Lit i)     = return i
exprEvaluation (Var v)     = do
    val <- asks (M.lookup v)
    case val of
      Just i  -> return i
      Nothing -> throwError $ NoVarBindingFound v
exprEvaluation (Add l r)   = liftM2 (+) (exprEvaluation l) (exprEvaluation r)
exprEvaluation (Sub l r)   = liftM2 (-) (exprEvaluation l) (exprEvaluation r)
exprEvaluation (Mul l r)   = liftM2 (*) (exprEvaluation l) (exprEvaluation r)
exprEvaluation (Div l r)   = do
    r' <- exprEvaluation r
    if r' == 0 then
      throwError DivizionByZero
    else do
      l' <- exprEvaluation l
      return $ div l' r'
exprEvaluation (Let v d e) = do
    d' <- exprEvaluation d
    local (M.insert v d') (exprEvaluation e)

evaluate :: Evaluator a -> Dictionary -> Either EvalError a
evaluate = runReaderT . runEvaluator

evaluateExpr :: Expr -> Dictionary -> Either EvalError Int
evaluateExpr = evaluate . exprEvaluation
