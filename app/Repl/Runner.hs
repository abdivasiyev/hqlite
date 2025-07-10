module Repl.Runner (
   run
) where

import System.Console.Haskeline

run :: IO ()
run = runInputT defaultSettings loop
   where
    loop :: InputT IO ()
    loop = do
       minput <- getInputLine "hqlite> "
       case minput of
          Nothing    -> return ()
          Just input -> runCmd loop $ parse input


data Command = Meta MetaCmd 
             | Sql String 
             | Unknown
 deriving (Show, Eq)

data MetaCmd = MetaExit | MetaUnknown deriving (Show, Eq)
 
parse :: String -> Command
parse [] = Unknown
parse s@(c:_) = case c of
   '.' -> Meta $ parseMeta s
   _   -> Sql s

parseMeta :: String -> MetaCmd
parseMeta s = case s of
   ".exit" -> MetaExit
   _       -> MetaUnknown

runCmd :: InputT IO () -> Command -> InputT IO ()
runCmd loop (Meta c) = do 
    runMeta c
    loop
runCmd loop (Sql s)  = do 
    runSql  s
    loop
runCmd loop Unknown  = do 
    outputStrLn $ "unrecognized command"
    loop

runMeta :: MetaCmd -> InputT IO ()
runMeta m = case m of
   MetaExit -> return ()
   MetaUnknown -> do outputStrLn $ "unrecognized meta command"

runSql :: String -> InputT IO ()
runSql s = do outputStrLn $ "you ran sql command"
