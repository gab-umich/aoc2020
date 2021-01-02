import Data.Set as Set

main :: IO ()
main = do
    f <- readFile "1.input"
    let list = Prelude.map rInt (lines f)
    let set = Set.fromList list
    let pairedList = [(list !! a, list !! b) | a <- [0 .. length list - 1], b <- [a .. length list - 1]]
    mapM_ (printProdIfMatch set) pairedList

rInt :: String -> Int
rInt = read

printProdIfMatch collection pair
                            | Set.member (2020 - x - y) collection = putStrLn $ "found " ++ show x ++ " x " ++ show y ++ " x " ++ show (2020 - x - y) ++ " = " ++ show ((2020 - x - y) * x * y) ++ "\n"
                            | otherwise = return ()
                            where x = fst pair
                                  y = snd pair