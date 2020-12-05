module Common.Utils where

import           System.Directory
import           System.IO
import           Text.Format

enumerateMultilineString :: String -> [((Int, Int), Char)]
enumerateMultilineString str
      | maximum lengths /= minimum lengths = error "Line lengths are not equal"
      | otherwise = zip coords (concat lines')
      where lines' = lines str
            xLength = length (head lines')
            yLength = length lines'
            lengths = map length lines'
            coords = [(x,y) | y <- [0..yLength -1], x <- [0..xLength - 1]]

readFileToString :: String -> IO String
readFileToString filePath = do
      handle <- openFile filePath ReadMode
      hGetContents handle

getInputFile :: Int -> IO String
getInputFile puzzleNumber = do
  workingDirectory <- getCurrentDirectory
  let path = format "{0}/res/AoC{1}/input.txt" [workingDirectory, show puzzleNumber]
  readFileToString path
