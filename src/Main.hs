module Main where

import ParserMini
import Codegen
import Emit

import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

import qualified LLVM.General.AST as AST

initModule :: Module2
initModule = emptyModule "my cool compiler"

process :: Module2 -> String -> IO (Maybe Module2, String)
process modo source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return (Nothing, "")
    Right ex -> do
      (ast, out) <- codegen modo ex
--      tt <- mapM (print . show) $ module2Definitions modo
      return (Just ast,out)

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
  loop mod = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        (modn, _) <- liftIO $ process mod input
        case modn of
          Just modn -> loop modn
          Nothing -> loop mod

processFile :: String -> IO (Maybe Module2)
processFile fname = do
    inp <- readFile fname
    (m, out) <- process initModule inp
    writeFile (fname ++ ".ll") out
    return m

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> processFile fname >> return ()
