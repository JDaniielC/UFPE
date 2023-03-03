main = do
       a <- getLine
       b <- getLine
       let result = editText a (read b)
       print result

data Cmd = Cursor Int
           | Backspace Int
           | Delete Int
           | Insert String
           deriving (Read)

editText :: String -> [Cmd] -> String
editText s [] = s
editText s (Cursor n:xs) = editText (moveCursor s n) xs
editText s (Backspace n:xs) = editText (backspace s n) xs
editText s (Delete n:xs) = editText (delete s n) xs
editText s (Insert str:xs) = editText (insert s str) xs

moveCursor :: String -> Int -> String
moveCursor s n = take n s ++ drop (n+1) s

backspace :: String -> Int -> String
backspace s n = take (n-1) s ++ drop n s

delete :: String -> Int -> String
delete s n = take n s ++ drop (n+1) s

insert :: String -> String -> String
insert s str = s ++ str

