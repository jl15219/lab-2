import System.Environment
import System.Process

data FalseTuple a = F a a

givenArray = ['a'..'z'] ++ ['A'..'Z']

forceFalseTuple :: FalseTuple a -> FalseTuple a
forceFalseTuple = id

instance Show a => Show (FalseTuple a) where
    show (F a b) = show a ++ "\n" ++ show b ++ "\n"

readFT :: [Char] -> FalseTuple Integer
readFT = readFT' [] [] where
    readFT' :: [Char] -> [Char] -> [Char] -> FalseTuple Integer
    readFT' a [] ('\n':c:rs) = readFT' a [c] rs
    readFT' a [] (s:as)      = readFT' (s:a) [] as
    readFT' a b ('\n':[])    = F ((read.reverse) a) ((read.reverse) b)
    readFT' a b (c:cs)       = readFT' a (c:b) cs

interactS :: FilePath -> String -> IO String
interactS f s = do (_,c,_) <- (readProcessWithExitCode f [] (s ++ "\n"))
                   return c

interactFT :: FilePath -> String -> IO (FalseTuple Integer)
interactFT f s = do c <- interactS f s
                    return (readFT c)

main :: IO ()
main = do (file:args) <- getArgs
          z <- interactFT file "Guess"
          putStr (show z)
          return ()

ifCorrect :: FalseTuple Integer -> Bool
ifCorrect (F _ 1) = True
ifCorrect (F _ _) = False

bruteForce :: [String]
bruteForce = concat [bruteForce' n | n <- [1..]]

bruteForce' :: Integer -> [String]
bruteForce' 1 = [[x] | x <- givenArray]
bruteForce' n = [(x:b) | x <- givenArray, b <- bruteForce' (n-1)]
