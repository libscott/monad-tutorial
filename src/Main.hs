module Main where



import Control.Monad.Counter


main :: IO ()
main = print (unCounter countyay 0)

countyay :: Counter Int
countyay = do
    a <- pure "betahaus"
    b <- pure " is warm"
    a <- pure "betahaus"
    a <- pure "betahaus"
    n <- Counter id
    b <- pure " is warm"
    b <- pure " is warm"
    return $ n

main2 :: IO ()
main2 = do
    name <- getName
    putStrLn (greet name)


getName :: IO String
getName = getLine


greet :: String -> String
greet name = "Hello, " ++ name ++ "!"
