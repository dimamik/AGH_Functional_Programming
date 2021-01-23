import Data.Char 
echo2 = getLine >>= \line -> putStrLn $ line ++ "!" 

fromStringToNewString :: String  -> String 
fromStringToNewString s = s ++ "!"

echo_ = getLine  >>= \line ->putStrLn $ fromStringToNewString line

doEcho2 = do 
  line <- getLine 
  putStrLn $ line ++ "!" 


echo3 :: IO () 
echo3 =  getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1 ++ l2 
 
dialog :: IO () 
dialog = putStr "What is your happy number? " 
         >> getLine 
         >>= \n -> let num = read n :: Int 
                  in
                   if num == 7 
                   then putStrLn "Ah, lucky 7!" 
                   else if odd num 
                        then putStrLn "Odd number! That's most people's choice..." 
                        else putStrLn "Hm, even number? Unusual!" 


doDialog = do
  putStr "What is your happy number? " 
  line <- getLine 
  let number = read line :: Int
    in 
      if number == 7 
                   then putStrLn "Ah, lucky 7!" 
                   else if odd number 
                        then putStrLn "Odd number! That's most people's choice..." 
                        else putStrLn "Hm, even number? Unusual!" 


doEcho3 = do
  l1 <- getLine 
  l2 <- getLine 
  putStrLn $ l1 ++ l2

twoQuestions :: IO () 
twoQuestions = do 
  putStr "What is your name? " 
  name <- getLine 
  putStr "How old are you? " 
  age <- getLine 
  print (name,age)

twoQuestions' = 
  putStr "What is your name? ">> 
  getLine >>= 
      \name -> 
        putStrLn  "How old are you? " >>
        getLine >>= \age -> 
          print (name,age)




newtype MyTriple a = MyTriple (a,a,a) deriving Show 

instance Functor MyTriple where
    fmap f (MyTriple (a,b,c)) = MyTriple (f a,f b,f c)
instance Applicative MyTriple where
    pure a = MyTriple (a,a,a)
    MyTriple (f, g, h) <*> MyTriple (x, y, z) = MyTriple (f x, g y, h z)


newtype Box a = MkBox a deriving Show 
 
instance Applicative Box where 
  pure = MkBox 
  (MkBox f) <*> w = fmap f w 
instance Functor Box where 
 fmap f (MkBox x) = MkBox (f x)


pure (*2) <*> MkBox 3 
(*2) <$> MkBox 3 
(+) <$> MkBox 1 <*> MkBox 2 
(++) <$> MkBox "abc" <*> MkBox "def" 
(\x y z -> (z,y,x)) <$> MkBox (Just 1) <*> MkBox (Just 2) <*> MkBox (Just 3)