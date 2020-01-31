-- Brendon Hale
-- S.I. #10037073

module A1 where

--import Test.QuickCheck

-- Enter 2 integers to find greatest common divisor between them
myGCD :: Integer -> Integer -> Integer
myGCD a b
   | b == 0 = a
   | otherwise = myGCD b (mod a b)

-- Returns list of divisors of positive integer
divisors :: Integer -> [Integer]
divisors n = [ div | div <- [1..n], mod n div == 0]

-- Checks whether a positive integer is prime and returns false if input is not positive
isPrime :: Integer -> Bool
isPrime n = divisors n == [1,n]

type Book = String
type Person = String
type Database = [(Person, Book)]

db :: Database
db = [("Alice","Tintin"),
      ("Anna", "Little Women"),
      ("Alice", "Asterix"),
      ("Rory", "Tintin")]

-- Creates a list of books a person is loaned
books :: Database -> Person -> [Book]
books db person = [ b | (p,b) <- db, p == person]

-- Crates a list of people that are loaned a book
borrowers :: Database -> Book -> [Person]
borrowers db book = [ p | (p,b) <- db, b == book]

-- Checks whether a book is associated in a loan
borrowed :: Database -> Book -> Bool
borrowed db book = [ b | (p,b) <- db, b == book] /= []

-- Returns the number of books a person has checked out
numBorrowed :: Database -> Person -> Int
numBorrowed db person = length [ b | (p,b) <- db, p == person]

-- Creates a new loan and adds it to the database
makeLoan :: Database -> Person -> Book -> Database
makeLoan db person book = [(person,book)] ++ db

-- Removes previously existing loan from database
returnLoan :: Database -> Person -> Book -> Database
returnLoan db person book = [(p,b) | (p,b) <- db, (p,b) /= (person,book)]

type Picture = [[Char]]

pic :: Picture
pic = [['.','#','#','.'],
       ['.','#','.','#'],
       ['.','#','#','#'],
       ['#','#','#','#']]

-- To print: mapM putStrLn pic

-- pg 162
-- Takes a picture as input and rotates it 90 degrees clockwise
--rotate90 :: Picture -> Picture
--rotate90 pic = flipH flipV pic
-- pick i'th element in each of the lines and reflect in a horizontally






