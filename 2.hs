import Data.Set as Set

main = do
    f <- readFile "1.input"
    let set = Set.fromList (Prelude.map rInt (lines f))
    mapM_ (printProdIfMatch set) set

rInt :: String -> Int
rInt = read

printProdIfMatch collection x
                            | Set.member (2020 - x) collection = putStrLn $ "found " ++ show x ++ " x " ++ show (2020 - x) ++ " = " ++ show ((2020 - x) * x) ++ ".\n"
                            | otherwise = return ()