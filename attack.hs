import System.Environment
import System.Process

data FalseTuple a = F a a

givenArray = ['a'..'z'] -- ++ ['A'..'Z']

forceFalseTuple :: FalseTuple a -> FalseTuple a
forceFalseTuple = id

instance Show a => Show (FalseTuple a) where
    show (F a b) = show a ++ "\n" ++ show b ++ "\n"

readFT :: [Char] -> FalseTuple Integer
readFT = readFT' [] [] where
    readFT' :: [Char] -> [Char] -> [Char] -> FalseTuple Integer
    readFT' a b [] = F ((read.reverse) a) ((read.reverse) b)
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
          z <- interactFT file "paaaaaaa"
          putStr (show z)
          --l <- lengthAttack file
          --putStr (show l ++ "\n")
          y <- sideChannel file
          putStr (y ++ "\n")
          return ()

ifCorrect :: FalseTuple Integer -> Bool
ifCorrect (F _ 1) = True
ifCorrect (F _ _) = False

bruteForce :: [String]
bruteForce = concat [bruteForce' n | n <- [1..]]

bruteForce' :: Integer -> [String]
bruteForce' 1 = [[x] | x <- givenArray]
bruteForce' n = [(x:b) | x <- givenArray, b <- bruteForce' (n-1)]

checkString :: FilePath -> String -> (IO String)
checkString f s = do z <- interactFT f s
                     if (ifCorrect z)
                        then return s
                        else return []

attemptBruteForce :: FilePath -> IO String
attemptBruteForce f = attemptBruteForce' f bruteForce

attemptBruteForce' :: FilePath -> [String] -> IO String
attemptBruteForce' f (x:xs) = do
  putStr (x ++ ", ")
  z <- interactFT f x
  if (ifCorrect z)
    then return x
    else attemptBruteForce' f xs

grabT :: FilePath -> String -> IO Integer
grabT f s = do (F t r) <- interactFT f s
               return t

charByCharAttack :: FilePath -> Int -> IO String
charByCharAttack = g "" 'a' where
  g :: String -> Char -> FilePath -> Int -> IO String
  g s c f n | n == (length s) = return s
            | otherwise = do
    time <- grabT f (s ++ (c:(take (n - (length s) - 1) ['a','a'..])))
    if (time > (((1+).fromIntegral .length) s))
      then g (s ++ [c]) 'a' f n
      else g s ([c..] !! 1) f n

lengthAttack :: FilePath -> IO Int
lengthAttack = g 1 where
  g :: Int -> FilePath -> IO Int
  g n f = do time <- grabT f (take n ['a','a'..])
             if (time > 0)
               then return n
               else g (n + 1) f

sideChannel :: FilePath -> IO String
sideChannel f = do
  pwrdLen <- lengthAttack f
  result <- charByCharAttack f pwrdLen
  return result
