module AoC25.ComboBreaker where

aoc25 :: IO ()
aoc25 = do
    print $ part1 2084668 3704642
    print "done"

data HandshakeParams = HP {
    subject :: Int,
    loopsize :: Int
}

part1 :: Int -> Int -> Int
part1 key1 key2 = handshake $ HP key2 key1LoopSize
    where key1LoopSize = crackLoopsize 7 key1

handshake :: HandshakeParams -> Int
handshake (HP subject loopsize) = iterate doLoop 1 !! loopsize
    where doLoop value = (value*subject) `mod` 20201227

crackLoopsize :: Int -> Int -> Int
crackLoopsize subject result = go 1 1
    where doLoop value = (value*subject) `mod` 20201227
          go count value = let next = doLoop value
                            in if next == result
                                then count
                                else go (count+1) next
