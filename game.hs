
removeItem :: String -> [String] -> [String]
removeItem a [] = []
removeItem a (x:xs) = if a == x then removeItem [] xs
                      else x : removeItem a xs
                        
addItem :: String -> [String] -> [String]
addItem a [] = [a]
addItem a (x:xs) = x : addItem a xs

printAllItems :: [String] -> IO()
printAllItems items = do
                        putStr "Mam : "
                        printItems items
                        
printItems :: [String] -> IO ()
printItems (x:xs) = do 
                    putStr x
                    if xs == [] then  putStr " \n"
                    else do
                            putStr ","
                            printItems xs 
                             

printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

readCommand :: IO String
readCommand = do
    putStr "> "
    xs <- getLine
    return xs
    
gameLoop items = do
          putStr "Hello World \n"
          putStr "AAA \n"
          putStr "\n"
          
          printAllItems items
          
          putStr "OK \n"
          
          cmd <- readCommand
          case cmd of 
                "add" -> do 
                            let nItems = addItem "a" items
                            gameLoop nItems
                "remove" -> do
                                let nItems = removeItem "a" items
                                gameLoop nItems
                _ -> do gameLoop items
          
main = do
        let items = ["PAC","TAC"]
        gameLoop items
