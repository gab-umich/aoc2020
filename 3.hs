import qualified Data.Set as Set

main = do
    f <- readFile "3.input"
    let result = foldl countTrees (0, 0, []) (lines f)
    print result
  where
    countTrees (count, index, list) line
        | (line !! index) == '#' = (count + 1, incre line index, list ++ ['#'])
        | otherwise              = (count, incre line index, list ++ ['.'])
        where incre line = flip mod (length line) . (+ 3)
