import qualified Data.Map hiding (map)
import qualified Data.Set as Set

main = do
    f <- readFile "2.input"
    let tuples = map parseLine (lines f)
    -- print tuples
    let result = foldr accu 0 tuples
    print result
  where
    parseLine line = convert (words line)
      where
        convert []     = (0, 0, 'a', "a")   -- debug
        convert [_]    = (1, 0, 'a', "a")   -- debug
        convert [_, _] = (2, 0, 'a', "a")   -- debug
        convert [rep, ch, pswd] =
            ( read (head (split rep)) :: Int
            , read (last (split rep)) :: Int
            , head ch
            , pswd
            )
        convert (_ : _ : _ : _ : _) = (4, 0, 'a', "a")    -- debug
    accu tup
        | -- basically an xor
          match_1 /= match_2 = (+ 1)
        | otherwise          = (+ 0)
      where
        (first, second, ch, pswd) = tup
        match_1                   = pswd !! (first - 1) == ch
        match_2                   = pswd !! (second - 1) == ch

split s = case dropWhile (== '-') s of
    "" -> []
    s' -> w : split s'' where (w, s'') = break (== '-') s'
