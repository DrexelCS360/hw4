module While (
    module While.Parser,
    module While.Syntax,

    run,
    runError
  ) where

import While.Env
import While.Parser
import While.ErrorInterpreter
import While.Interpreter
import While.Syntax

run :: String -> IO State
run s =
  case parseWhile s of
    Nothing  -> fail $ "Cannot parse: " ++ s
    Just stm -> return $ While.Interpreter.evalS stm emptyState

runError :: String -> IO (Maybe State)
runError s =
  case parseWhile s of
    Nothing  -> fail $ "Cannot parse: " ++ s
    Just stm -> return $ While.ErrorInterpreter.evalS stm emptyState

