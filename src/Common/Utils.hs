module Common.Utils where

enumerateMultilineString :: String -> [((Int, Int), Char)]
enumerateMultilineString str
      | maximum lengths /= minimum lengths = error "Line lengths are not equal"
      | otherwise = zip coords (concat lines')
      where lines' = lines str
            xLength = length (head lines')
            yLength = length lines'
            lengths = map length lines'
            coords = [(x,y) | y <- [0..yLength -1], x <- [0..xLength - 1]]