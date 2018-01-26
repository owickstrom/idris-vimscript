module Main where

import           Control.Monad      (void)

import           Idris.AbsSyntax
import           Idris.ElabDecls
import           Idris.Main
import           Idris.Options

import           IRTS.CodegenVim
import           IRTS.Compiler

import           System.Environment
import           System.Exit
import qualified Vimscript.Optimise as Optimise

data Opts = Opts
  { inputs :: [FilePath]
  , output :: FilePath
  , flags  :: Optimise.Flags
  } deriving (Show)

showUsage :: IO ()
showUsage = do
  putStrLn "Usage: idris-codegen-vim <ibc-files> [-o <output-file>] [flags]"
  putStrLn ""
  putStrLn "Available flags:"
  putStrLn "  --disable-dead-code-elimination : Disable dead code elimination"
  putStrLn "  --disable-tail-call-optimisation : Disable tail call optimisation"
  exitSuccess

getOpts :: IO Opts
getOpts = do
  xs <- getArgs
  return $ process (Opts [] "main.vim" Optimise.defaultFlags) xs
  where
    process opts ("-o":o:xs) = process (opts {output = o}) xs
    process opts ("--disable-dead-code-elimination":xs) =
      process (opts {flags = flags''}) xs
      where
        flags' = flags opts
        flags'' = flags' {Optimise.dce = False}
    process opts ("--disable-tail-call-optimisation":xs) =
      process (opts {flags = flags''}) xs
      where
        flags' = flags opts
        flags'' = flags' {Optimise.tco = False}
    process opts (x:xs) = process (opts {inputs = x : inputs opts}) xs
    process opts [] = opts

mainWithOpts :: Opts -> Idris ()
mainWithOpts opts = do
  elabPrims
  void (loadInputs (inputs opts) Nothing)
  mainProg <- elabMain
  ir <- compile (Via IBCFormat "vim") (output opts) (Just mainProg)
  runIO $ codegenVim (flags opts) ir

main :: IO ()
main = do
  opts <- getOpts
  if null (inputs opts)
    then showUsage
    else runMain (mainWithOpts opts)
