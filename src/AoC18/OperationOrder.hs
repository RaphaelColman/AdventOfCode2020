module AoC18.OperationOrder where

import Common.Utils

aoc18 :: IO ()
aoc18 = do
    contents <- getInputFile 18
    print contents


parseContents :: String -> Int
parseContents s = undefined

infixl 7 @+
(@+) = (+)

infixl 6 @*
(@*) = (*)
