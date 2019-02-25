module Main where

import System.Console.GetOpt
import System.Environment
import System.IO

import While

import qualified While.Interpreter as I
import qualified While.ErrorInterpreter as E

main :: IO ()
main = do
    (flags, files) <- getArgs >>= parseOpts
    let interp = chooseInterpreter flags
    mapM_ (runInterp interp) files
  where
    chooseInterpreter :: [Flag] -> Stm -> IO ()
    chooseInterpreter flags stm
        | ErrorInterpreter `elem` flags    = print $ E.evalS stm E.emptyState
        | otherwise                        = print $ I.evalS stm I.emptyState

    runInterp :: (Stm -> IO ()) -> FilePath -> IO ()
    runInterp interp path = do
        s <- readFile path
        let maybe_stm = parseWhile s
        case maybe_stm of
          Nothing  -> hPutStrLn stderr $ "Could not parse '" ++ path ++ "'"
          Just stm -> interp stm

options :: [OptDescr Flag]
options =
 [ Option ['e'] ["error"] (NoArg ErrorInterpreter) "run the error interpreter"
 ]

parseOpts :: [String] -> IO ([Flag], [String])
parseOpts argv =
    case getOpt Permute options argv of
      (o,args,[]) -> return (o,args)
      (_,_,errs)  -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: while [OPTION...] FILES"

data Flag = ErrorInterpreter
  deriving (Eq, Show)
