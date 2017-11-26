{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interpreter where

import           Structure
import           Pipe
import           Evaluator

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Cont
import qualified Data.Map.Lazy as M

data InterpreterError = EvaluationError  EvalError
                      | VarRedefError    Ref
                      | NoVarUpdateError Ref
    deriving (Eq, Show)

data InterpreterErrorInfo = ErrInfo Statement InterpreterError
  deriving (Eq, Show)

type MonadPureInterpreter  m = ( MonadState Dictionary m
                               , MonadError InterpreterError m
                               )

type MonadInterpreter m = ( MonadState Dictionary m
                          , MonadError InterpreterErrorInfo m
                          , MonadPipe Int m
                          , MonadCont m
                          , MonadReader (m ()) m
                          )

exprInterpreter :: MonadPureInterpreter m => Expr -> m Int
exprInterpreter e = do
    dict <- get
    case evaluate (exprEvaluation e) dict of
      Left er -> throwError $ EvaluationError er
      Right x -> return x


newVarInterpreter :: MonadPureInterpreter m => Ref -> Int -> m ()
newVarInterpreter v i = do
    old_i <- gets (M.lookup v)
    case old_i of
      Just _  -> throwError $ VarRedefError v
      Nothing -> modify (M.insert v i)

updVarInterpreter :: MonadPureInterpreter m => Ref -> Int -> m ()
updVarInterpreter v i = do
    old_i <- gets (M.lookup v)
    case old_i of
      Just _  -> modify (M.insert v i)
      Nothing -> throwError $ NoVarUpdateError v

rangeInterpreter :: MonadState Dictionary m => Ref -> Int -> Int -> m () -> m ()
rangeInterpreter v begin end block = mapM_ (\i -> modify (M.insert v i) >> block) [begin..end]

withExceptT' :: MonadError e m => (e' -> e) -> ExceptT e' m a -> m a
withExceptT' f = either (throwError . f) return <=< runExceptT

withErrorInfo :: (MonadError InterpreterErrorInfo m) => Statement -> ExceptT InterpreterError m a -> m a
withErrorInfo = withExceptT' . ErrInfo

statementInterpreter :: MonadInterpreter m => Statement -> m ()
statementInterpreter st = impl st
  where
    impl (NewVarStatement v e) = do
        i <- exprInterpreter' e
        newVarInterpreter' v i
    impl (UpdVarStatement v e) = do
        i <- exprInterpreter' e
        updVarInterpreter' v i
    impl (PrintStatement e) = do
        i <- exprInterpreter' e
        writeValue i
    impl (ReadStatement v) = do
        i <- readValue
        updVarInterpreter' v i
    impl (ForStatement v begin end stmts) = do
        newVarInterpreter' v 0
        begin' <- exprInterpreter' begin
        end' <- exprInterpreter' end
        pushCC $ rangeInterpreter v begin' end' $ statementsInterpreter stmts
    impl (IfStatement cond stmts) = do
        cond' <- exprInterpreter' cond
        when (cond' /= 0) $ statementsInterpreter stmts
    impl BreakStatement = join ask

    exprInterpreter'   e   = withErrorInfo st $ exprInterpreter   e
    updVarInterpreter' r i = withErrorInfo st $ updVarInterpreter r i
    newVarInterpreter' r i = withErrorInfo st $ newVarInterpreter r i

pushCC :: (MonadCont m, MonadReader (m a) m) => m () -> m ()
pushCC m = callCC $ \b -> local (const . b $ ()) m

statementsInterpreter :: MonadInterpreter m => [Statement] -> m ()
statementsInterpreter = mapM_ statementInterpreter

programInterpreter :: MonadInterpreter m => Program -> m ()
programInterpreter = pushCC . statementsInterpreter

newtype Interpreter pipe a = Interpreter { runInterpreter :: StateT Dictionary (ExceptT InterpreterErrorInfo (ContT (Either InterpreterErrorInfo Dictionary) (ReaderT (Interpreter pipe ()) pipe))) a}
  deriving (Functor, Applicative, Monad, MonadState Dictionary, MonadError InterpreterErrorInfo, MonadCont, MonadReader (Interpreter pipe ()))

instance MonadPipe Int pipe => MonadPipe Int (Interpreter pipe) where
  readValue  = Interpreter . lift . lift . lift . lift $ readValue
  writeValue = Interpreter . lift . lift . lift . lift . writeValue

interpret :: MonadPipe Int pipe => Interpreter pipe a -> pipe (Either InterpreterErrorInfo Dictionary)
interpret =  flip runReaderT (return ()) . flip runContT return . runExceptT . flip execStateT M.empty . runInterpreter

interpretIO :: Interpreter (IOPipe Int) a -> IO (Either InterpreterErrorInfo Dictionary)
interpretIO = runIO . interpret

interpretList :: Interpreter (ListPipe Int) a -> [Int] -> (Either InterpreterErrorInfo Dictionary, [Int])
interpretList p input = f . flip runState (input, []) . runList . interpret $ p
  where
    f (e, (_, w)) = (e, w)
