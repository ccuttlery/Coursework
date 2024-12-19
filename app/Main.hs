{-# LANGUAGE InstanceSigs #-}

module Main where
import System.Environment
import System.IO
import System.Exit (exitSuccess)
import Data.Char (toLower)
import Data.List (sortBy)
import Test.HUnit
import qualified Control.Monad


{----- Custom Data Types -----}
data User = User {
    userId :: Int,
    name :: String
} deriving (Eq)
instance Show User where
    show :: User -> String
    show user = name user ++ " ["++show (userId user)++"]"


data Status = Available | Borrowed User
    deriving (Eq)
instance Show Status where
    show :: Status -> String
    show Available = "Available"
    show (Borrowed user) = "Borrowed by " ++ show user

statusToJson :: Status -> String
statusToJson Available = "-1"
statusToJson (Borrowed user) = show (userId user)

jsonToStatus :: String -> Status
jsonToStatus "-1" = Available
jsonToStatus x = Borrowed (users!!read x)


data Book = Book {
    bookId :: Int,
    title :: String,
    authorFirstname :: String,
    authorLastname :: String,
    status :: Status
} deriving (Eq)
instance Show Book where
    show :: Book -> String
    show book = "[" ++ show (bookId book) ++ "] " ++ title book ++ " by " ++ authorFirstname book ++ " " ++ authorLastname book ++ " (" ++ show (status book) ++ ")"

bookToJson :: Book -> String
bookToJson book = "["++show (bookId book)++",\""++title book++"\",\""++authorFirstname book++"\",\""++authorLastname book++"\","++statusToJson (status book)++"]"

booksToJson :: [Book] -> String
booksToJson [x] = bookToJson x ++ "\n"
booksToJson (x:xs) = bookToJson x ++ ",\n" ++ booksToJson xs

jsonToBook :: String -> IO Book
jsonToBook json = do
    let values = words (remove "[\"]" (replace ',' ' ' (replace ' ' '_' json)))
    return Book {bookId=read (head values), title=replace '_' ' ' (values!!1), authorFirstname=values!!2, authorLastname=values!!3, status=jsonToStatus (values!!4)}

jsonToBooks :: [String] -> IO [Book]
jsonToBooks [] = return []
jsonToBooks (x:xs) = do
    b <- jsonToBook x
    remainder <- jsonToBooks xs
    return (b : remainder)



{----- Constants -----}
users :: [User]
users = [
    User {userId=0, name="Admin"},
    User {userId=1, name="Brian"},
    User {userId=2, name="Charlie"},
    User {userId=3, name="Declan"}]


bookpath :: String
bookpath = "resources/books.json"


validCommands :: [String]
validCommands = ["add", "remove", "borrow", "return", "list", "users", "help", "save", "quit", "quitno"]



{----- Utility Functions -----}
-- Removes all instances of any character in the first string from the second string
remove :: String -> String -> String
remove _ [] = ""
remove filter (x:xs) =
    if x `elem` filter then remove filter xs
    else x : remove filter xs


-- Replaces all instances of character a in the string with character b
replace :: Char -> Char -> String -> String
replace _ _ [] = ""
replace a b (x:xs)
    | x == a = b : replace a b xs
    | otherwise = x : replace a b xs


-- Like words, but fancier!
-- (Used for formatting the input to a form the addBooks function can use)
{- ex: 

fancyWords "add 'Animal Farm' George Orwell"
-> ["add", "Animal Farm", "George", "Orwell"]

whereas words would produce
-> ["add", "'Animal", "Farm'", "George", "Orwell"]
-}
fancyWords :: String -> IO [String]
fancyWords "add" = return ["add"]
fancyWords str = do
    let x = break (=='\'') str
    let cmd = fst x

    let y = break (=='\'') (tail (snd x))
    let title = fst y

    let name = tail (words (snd y))


    return ([init cmd, title] ++ name)


-- Format the input string, depending on whether it's an 'add' command or not
format :: String -> IO [String]
format "" = return [""]
format str
    | head (words (map toLower str)) == "add" = fancyWords str
    | otherwise = return (words (map toLower str))


-- Calculates the lowest integer which isn't already
-- used as a bookId, starting from n
calculateNextId :: Int -> [Book] -> Int
calculateNextId n books =
    if n `elem` map bookId books
    then calculateNextId (n+1) books
    else n


writeToFile :: String -> String -> IO ()
writeToFile filepath str = do
    f <- openFile filepath WriteMode
    hPutStr f str
    hClose f


{----- Main -----}
main :: IO ()
main = do
    putStrLn "\n--------------------------------------------"
    putStrLn "| Welcome to the Library Management System |"
    putStrLn "--------------------------------------------"
    putStrLn "\nUse 'help' for a list of all valid commands."
    putStrLn "Enter a command without its arguments to learn more."

    contents <- readFile bookpath
    books <- jsonToBooks ((init.tail) (lines contents))

    loop books


updateBooks :: [String] -> [Book] -> [Book]
updateBooks ("add":args) = addBook args
updateBooks ("remove":args) = removeBook args
updateBooks ("borrow":args) = borrowBook args
updateBooks ("return":args) = returnBook args
updateBooks (_:args) = id


loop :: [Book] -> IO ()
loop books = do
    putStrLn "\nInput command: "
    input <- getLine

    -- Add function needs slightly different formatting.
    args <- format input

    case head args of
        -- list [all|available|borrowed] [id|author]
        "list" -> if length args == 3 then listBooks (tail args) books else showUnknownCommand (head args)
        "users" -> showAll users
        "help" -> showCommands
        "save" -> do
            putStrLn "\nAre you sure you want to save all changes? (Can't be undone) [Y/N]"
            input <- getLine
            Control.Monad.when (map toLower input == "y") $ do
                writeToFile bookpath ("[\n" ++ booksToJson books ++ "]")
                putStrLn "\nChanges saved!"
        "quit" -> do
            putStrLn "\nAre you sure you want to save all changes and quit? [Y/N]"
            input <- getLine
            Control.Monad.when (map toLower input == "y") $ do
                writeToFile bookpath ("[\n" ++ booksToJson books ++ "]")
                putStrLn "\nChanges saved, come back soon!"
                exitSuccess
        "quitno" -> do
            putStrLn "\nAre you sure you want to quit without saving? [Y/N]"
            input <- getLine
            Control.Monad.when (map toLower input == "y") $ do
                putStrLn "\nNo changes saved, come back soon!"
                exitSuccess

        _ -> Control.Monad.when (head args `notElem` validCommands || length args <= 1) $ showUnknownCommand (head args)


    -- if command is incomplete, don't try to call a book-related function
    if length args > 1
    then loop (sortBy (\a b -> compare (bookId a) (bookId b)) (updateBooks args books))
    else loop books



{----- Show Functions -----}
showUnknownCommand :: String -> IO ()
showUnknownCommand cmd = do
    putStrLn ""
    case cmd of
        "add" -> do
            putStrLn "add ['title'] [authorFirstName] [authorLastName] - add a book to the library\n"
            putStrLn "  To add a book, wrap it's title in apostrophes 'like this' and seperate the author's first and last name with a space."
            putStrLn "  e.g.: <add 'Animal Farm' George Orwell>"
            putStrLn "  The bookId will be generated automatically."

        "remove" -> do
            putStrLn "remove [bookId] - remove a book from the library\n"
            putStrLn "  e.g.: <remove 0>"
            putStrLn "  You can use 'list' to see all book ids."
            putStrLn "  Warning! Once you remove a book it is gone forever!"

        "borrow" -> do
            putStrLn "borrow [bookId] [userId] - borrow a book for a user\n"
            putStrLn "  e.g.: <borrow 1 1>"
            putStrLn "  You can only borrow an unborrowed book! Use 'list' to see all of these."
            putStrLn "  You can use 'users' to see all user ids."

        "return" -> do
            putStrLn "return [bookId] - return a currently borrowed book\n"
            putStrLn "  e.g.: <return 2>"
            putStrLn "  You can only return a borrowed book! Use 'list' to see all of these."

        "list" -> do
            putStrLn "list [all|available|borrowed] [id|author] - show a list of books in the library\n"
            putStrLn "  Shows a list of all the books, or a list of only the available/borrowed ones."
            putStrLn "  You can choose to order by bookId or by author surname (alphabetically)."
            putStrLn "  e.g.: <list all id> or <list available author>"

        cmd -> putStr ("Unknown or incomplete command: '"++cmd++"'. ")

    putStrLn "\nUse 'help' for a list of all valid commands."



-- Prints out each element of the given list on a seperate line
showAll :: Show a => [a] -> IO ()
showAll [] = return ()
showAll (x:xs) = do
    print x
    showAll xs

-- Like showAll, but only prints an element if some predicate p is true for it
showAllIf :: Show a => (a -> Bool) -> [a] -> IO ()
showAllIf _ [] = return ()
showAllIf p (x:xs) = do
    Control.Monad.when (p x) $ print x
    showAllIf p xs


showCommands :: IO ()
showCommands = do
    putStrLn "\nCommands:"
    putStrLn "add ['title'] [authorFirstName] [authorLastName] - add a new book to the library (make sure to wrap title in 'quotes')"
    putStrLn "remove [bookId] - remove a book from the library"
    putStrLn "borrow [bookId] [userId] - borrow a book for a user"
    putStrLn "return [bookId] - return a currently borrowed book"
    putStrLn "list [all|available|borrowed] [id|author] - show a list of books in the library (ordered by id or author surname alphabetically)"
    putStrLn "users - show a list of all registered users"
    putStrLn "help - show this list of commands"
    putStrLn "quit - save changes and exit the program"
    putStrLn "quitno - exit the program without saving changes"



{----- Library Management -----}
-- add ['title'] [authorFirstname] [authorLastname]
addBook :: [String] -> [Book] -> [Book]
addBook (title:fname:lname:_) books = books ++ [Book {bookId=calculateNextId 0 books, title=title, authorFirstname=fname, authorLastname=lname, status=Available}]

-- remove [bookId]
removeBook :: [String] -> [Book] -> [Book]
removeBook (bId:_) books = [b | b <- books, bookId b /= read bId]

-- borrow [bookId] [userId]
borrowBook :: [String] -> [Book] -> [Book]
borrowBook (bId:uId:_) = map func where
    func book =
        if status book == Available && bookId book == read bId
        then Book {bookId=bookId book, title=title book, authorFirstname=authorFirstname book,
                authorLastname=authorLastname book, status=Borrowed (users!!read uId)}
        else book

-- return [bookId]
returnBook :: [String] -> [Book] -> [Book]
returnBook (bId:_) = map func where
    func book =
        if bookId book == read bId
        then Book {bookId=bookId book, title=title book, authorFirstname=authorFirstname book,
                authorLastname=authorLastname book, status=Available}
        else book

-- list [all|available|borrowed] [id|author]
listBooks :: [String] -> [Book] -> IO ()
listBooks (x:"author":_) books =
    listBooks [x] (sortBy (\a b -> compare (authorLastname a) (authorLastname b)) books)
listBooks (x:"id":_) books =
    listBooks [x] (sortBy (\a b -> compare (bookId a) (bookId b)) books)
listBooks ("all":_) books = showAll books
listBooks ("available":_) books = showAllIf (\x -> status x == Available) books
listBooks ("borrowed":_) books = showAllIf (\x -> status x /= Available) books



{----- Testing -----}
testBooks :: [Book]
testBooks = [
    Book {bookId=0, title="Animal Farm", authorFirstname="George",
        authorLastname="Orwell", status=Available},
    Book {bookId=1, title="Lord of the Flies", authorFirstname="William",
        authorLastname="Golding", status=Available},
    Book {bookId=2, title="Star Maker", authorFirstname="Olaf",
        authorLastname="Stapledon", status=Borrowed (head users)}]

f451args :: [String]
f451args = ["Fahrenheit 451", "Ray", "Bradbury"]

f451 :: Book
f451 = Book {bookId=3, title="Fahrenheit 451", authorFirstname="Ray",
    authorLastname="Bradbury", status=Available}


tests :: Test
tests = TestList [
    -- 0) Lowest unused bookId should be 3
    TestCase (assertEqual "Generate new unique bookId" 3 (calculateNextId 0 testBooks)),
    -- 1) Adding book from args should be the same as ++ book to the list
    TestCase (assertEqual "Add new book (Fahrenheit 451)" (testBooks ++ [f451]) (addBook f451args testBooks)),
    -- 2) Removing the book with id 0 (ie. the first book) should be the same as tail
    TestCase (assertEqual "Remove book with id 0" (tail testBooks) (removeBook ["0"] testBooks)),
    -- 3) addBook and removeBook are essentially the inverse of each other
    -- Hence, adding and then removing should return the original list
    TestCase (assertEqual "Add then remove same book" testBooks ((removeBook ["3"].addBook f451args) testBooks)),
    -- 4) Trying to remove a book that doesn't exist should do nothing
    TestCase (assertEqual "Remove non-existent book" testBooks (removeBook ["5"] testBooks)),
    -- 5) Test borrowing
    TestCase (assertEqual "User 0 borrows book 1" (Borrowed (head users)) (status (borrowBook ["1", "0"] testBooks!!1))),
    -- 6) Attempting to borrow an already borrowed book should leave it unchanged
    TestCase (assertEqual "Try to borrow unavailable book" (Borrowed (head users)) (status (borrowBook ["2", "1"] testBooks!!2))),
    -- 7) Test returning borrowed book
    TestCase (assertEqual "Return book 2" Available (status (returnBook ["2"] testBooks!!2))),
    -- 8) Test returning already available book
    TestCase (assertEqual "Return book 1 (already available)" Available (status (returnBook ["1"] testBooks!!1))),
    -- 9) Test that borrowing and returning are the inverse
    TestCase (assertEqual "Borrow and then return same book" (status (head testBooks)) (status (head (returnBook ["0"] (borrowBook ["0", "1"] testBooks)))))]


-- runTestTT tests