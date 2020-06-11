import Test.QuickCheck
import qualified Data.Text as T
import Data.List
import Data.Char
import Control.Monad 
import Control.Exception
import Data.Time
import System.Clock

positions :: (Ord a, Num a) => a -> a -> [a]
positions start end = takeWhile (<=end) $ iterate (+1) start


failure_table :: Eq a => [a] -> [Int]
failure_table pattern =
    build pattern default_values initial_posInArray initial_posInPattern
    where
        default_values = [-1, 0]
        initial_posInArray = 2
        initial_posInPattern = 0
        build pattern failureArray posInArray posInPattern = 
            if posInArray >= length pattern
            then failureArray
            else
                if (pattern!!(posInArray - 1)) == (pattern!!posInPattern)
                then build pattern (failureArray ++ [posInPattern + 1]) (posInArray + 1) (posInPattern + 1)
                else if posInPattern > 0
                     then build pattern failureArray posInArray (failureArray!!posInPattern)
                     else build pattern (failureArray ++[0]) (posInArray + 1) posInPattern


kmp :: Eq a => [a] -> [a] -> Maybe Int
kmp pattern text =
    check pattern text initial_posInPattern initial_posInText 
    where
        initial_posInPattern = 0
        initial_posInText = 0
        check pattern text i j =
            if pattern == []
            then Nothing
            else
            if (i < length text)
            then
                if (j/= (-1) && pattern!!j /= text!!i)
                then check pattern text i ((failure_table pattern)!!j)
                else
                    if j == (length pattern - 1)
                    then Just (i - length pattern + 1)
                    else check pattern text (i+1) (j+1)
            else Nothing


kmp2 :: Eq a => [a] -> [a] -> [Int]
kmp2 pattern text =
    check pattern text initial_posInPattern initial_posInText 
    where
        initial_posInPattern = 0
        initial_posInText = 0
        check pattern text i j =
            if pattern == []
            then []
            else
            if (i < length text)
            then
                if (j/= (-1) && pattern!!j /= text!!i)
                then check pattern text i ((failure_table pattern)!!j)
                else
                    if j == (length pattern - 1)
                    then (positions (i - length pattern + 1) i)
                    else check pattern text (i+1) (j+1)
             else []


kmp3 :: Eq a => [a] -> [a] -> Bool
kmp3 pattern text=
    check pattern text initial_posInPattern initial_posInText 
    where
        initial_posInPattern = 0
        initial_posInText = 0
        check pattern text i j =
            if pattern == []
                then True
            else
            if (i < length text)
            then
                if (j/= (-1) && pattern!!j /= text!!i)
                then check pattern text i ((failure_table pattern)!!j)
                else
                    if j == (length pattern - 1)
                    then True
                    else check pattern text (i+1) (j+1)
             else False


kmp4 :: Eq a => [a] -> [a] -> [Int]
kmp4 pattern text =
    check pattern text initial_posInPattern initial_posInText array 
    where
        array = []
        initial_posInPattern = 0
        initial_posInText = 0
        check pattern text i j array=
            if pattern == []
            then []
            else
            if (i < length text)
            then
                if (j/= (-1) && pattern!!j /= text!!i)
                then check pattern text i ((failure_table pattern)!!j) array
                else
                    if j == (length pattern - 1)
                    then
                        if length pattern /= 1
                        then check pattern text i 0 (array ++[(i - length pattern + 1)])
                        else check pattern text (i+1) 0 (array ++[(i - length pattern + 1)])
                    else check pattern text (i+1) (j+1) array
            else array


--pattern: "aaaaaaaaab"
--text: "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

--pattern : o mie
--text: 1 milion/ 10 milioane

lettersAfter (x:xs) number number2 listToReturn = lettersAfter xs number number2 (listToReturn ++ [number - x - number2])
lettersAfter [] number number2 listToReturn = listToReturn

--prop_revapp :: [Char] -> [Char] -> Bool
--prop_revapp xs ys =map(+var) (kmp2 (reverse xs) (reverse ys)) == kmp2 xs ys
   -- where var = length ys - (kmp2 xs ys)!!(length xs - 1) -1

prop_rev :: [Char] -> [Char] -> Bool
prop_rev xs ys = sort (kmp4 xs ys)  == sort (lettersAfter (kmp4 (reverse xs) (reverse ys)) (length ys) (length xs) [])


prop_rev2 :: [Char] -> [Char] ->Bool
prop_rev2 xs ys = sort (lettersAfter (kmp4 xs ys) (length ys) (length xs) []) == sort (kmp4 (reverse xs) (reverse ys))

prop_reverse :: [Char] -> [Char] ->Bool
prop_reverse xs ys = kmp3 xs ys == kmp3 (reverse xs) (reverse ys)

prop_contains :: [Char] -> [Char] -> Bool
prop_contains xs ys = kmp3 xs ys == T.isInfixOf (T.pack xs) (T.pack ys)

prop_assert :: [Char] -> [Char] -> Bool
prop_assert xs ys = kmp "ababaca" "abcbababababacab" == Just 8

--test1 = quickCheckWith stdArgs { maxSuccess = 500 } prop_revapp
test2 = quickCheckWith stdArgs { maxSuccess = 500 } prop_reverse
test3 = quickCheckWith stdArgs { maxSuccess = 500 } prop_contains
test4 = quickCheckWith stdArgs { maxSuccess = 1 } prop_assert
test5 = quickCheckWith stdArgs { maxSuccess = 500 } prop_rev
test6 = quickCheckWith stdArgs { maxSuccess = 100 } prop_rev2

createInputPattern :: [Char] -> Int -> [Char]
createInputPattern str 0 = str ++ "b"
createInputPattern str nr = createInputPattern (str ++ "a") (nr - 1)

createInputText :: [Char] -> Int -> [Char]
createInputText str 0 = str ++ "a"
createInputText str nr = createInputText (str ++ "a") (nr - 1)

main = do
    loop 10 10 
    where
        loop x y = do
            let pattern = createInputPattern [] x
            let text = createInputText [] y
            start <- getCurrentTime
            putStrLn $ "The result for pattern of length " ++ show x ++ " in text of length " ++ show y ++ "is:   " ++ show (kmp4 pattern text)
            end <- getCurrentTime
            print (diffUTCTime end start)
            loop (x*10) (y*100)