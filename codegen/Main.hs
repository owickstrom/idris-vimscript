module Main where

import           Control.Monad      (void)

import           Idris.AbsSyntax
import           Idris.ElabDecls
import           Idris.Main

import           IRTS.CodegenVim
import           IRTS.Compiler

import           System.Environment
import           System.Exit

data Opts = Opts { inputs :: [FilePath],
                   output :: FilePath }

showUsage :: IO ()
showUsage = do
  putStrLn "Usage: idris-codegen-vim <ibc-files> [-o <output-file>]"
  exitSuccess

getOpts :: IO Opts
getOpts = do
  xs <- getArgs
  return $ process (Opts [] "main.vim") xs
  where
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts (x:xs)      = process (opts { inputs = x:inputs opts }) xs
    process opts []          = opts

mainWithOpts :: Opts -> Idris ()
mainWithOpts opts = do
  elabPrims
  void (loadInputs (inputs opts) Nothing)
  mainProg <- elabMain
  ir <- compile (Via IBCFormat "vim") (output opts) (Just mainProg)
  runIO $ codegenVim ir

main :: IO ()
main = do
  opts <- getOpts
  if null (inputs opts)
      then showUsage
      else runMain (mainWithOpts opts)


