import qualified Data.Set as Set

main = do
    f <- readFile "3-2.test"
    let result = foldr countTrees (0, 1) (lines f)
    print result
  where
    countTrees line (count, index)
        | (line !! index) == '#' = (count + 1, incre line index)
        | otherwise              = (count, incre line index)
        where incre line = mod (length line) . (+ 3)
