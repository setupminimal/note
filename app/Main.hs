module Main where

import Prelude hiding (lookup)
import System.Environment (getArgs)
import Data.List (intercalate, (\\))
import System.Directory
import System.FilePath
import Data.Map

data Command a = List a | Init a | Also (Command a) a | That a | Help a

commands = fromList [ ("list", List)
                    , ("show", List)
                    , ("init", Init)
                    , ("initialize", Init)
                    , ("that", That)
                    , ("help", Help)
                    ]

type Note = String

helpText = intercalate "\n" [
    "Help:"
  , "note - show all the notes"
  , "note init - create a new .notes file"
  , "note that [words] - add a new note"
  , "note also that [words] - add a sub-note"
  , "note also also that [words] - add a sub-sub-note"
  , "    (etc.)"
  , "note list - show the notes"
  , "note help - show this message"
                            ]

main :: IO ()
main = do
  args <- getArgs
  noteFile <- findNoteFile
  doCmd (getCommand args) noteFile

getCommand [] = List []
getCommand ["that"] = Help [] -- possibly open editor?
getCommand [cmd] = case lookup cmd commands of
                     Nothing -> Help []
                     Just a -> a []
getCommand ("also":as) = Also (getCommand as) []
getCommand (cmd:ws) = case lookup cmd commands of
                          Just a -> a ws
                          Nothing -> That $ cmd:ws

doCmd :: Command [String] -> Maybe FilePath -> IO ()
doCmd (That words) (Just noteFile) = appendFile noteFile ("* " ++ unwords words ++ "\n")
doCmd (List _) (Just noteFile) = readFile noteFile >>= putStr
doCmd (Init _) (Just noteFile) = putStrLn $ "There already seems to be a note file at: " ++ show noteFile
doCmd (Init _) Nothing = writeFile "./.notes" "" >> putStrLn "Created empty .notes file"
doCmd (Also cmd _) (Just noteFile) = appendFile noteFile "*" >> doCmd cmd (Just noteFile)
doCmd (Help words) _ = (if length words /= 0 then putStrLn $ "I don't know how to " ++ unwords words ++ ".\n" else return ()) >> putStrLn helpText
doCmd _ Nothing = putStrLn "There doesn't seem to be a .notes file here - try 'note init'."

findNoteFile = getCurrentDirectory >>= canonicalizePath >>= findNoteFile'

findNoteFile' :: FilePath -> IO (Maybe FilePath)
findNoteFile' "/" = return Nothing
findNoteFile' dir = do
  x <- doesFileExist (dir </> ".notes")
  if x then
    return $ return $ dir </> ".notes" else
    do
      parent dir >>= findNoteFile'

parent dir = canonicalizePath $ dir </> ".."
