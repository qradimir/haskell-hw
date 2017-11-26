module Main where

import           Parser
import           Interpreter
import           PrettyShow

import           System.Environment
import           Text.Megaparsec.Error
import           Control.Monad.Cont
import qualified Data.Text.Lazy.IO as T


main :: IO ()
main = flip runContT return $ callCC $ \exit -> do
  [f] <- liftIO getArgs
  s <- liftIO . T.readFile $ f
  prog <- either (liftIO . putStrLn . parseErrorPretty >=> exit) return $ parseProgram s
  res <- liftIO . interpretIO . programInterpreter $ prog
  either (liftIO . putStrLn . prettyShow) (const . return $ ()) res
