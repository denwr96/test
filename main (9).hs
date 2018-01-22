showTODOList :: (Int, String) -> IO ()
showTODOList (n, todo) = putStrLn (show n ++ ": " ++ todo)

prompt :: [String] -> IO ()
prompt todos = do
    command <- getLine
    handleCommand command todos

handleCommand :: String -> [String] -> IO ()
handleCommand ('+':' ':todo) todos = prompt (todo:todos)
handleCommand ('-':' ':num ) todos =
    case delete (read num) todos of
        Nothing -> do
            putStrLn "Todo number not found"
            prompt todos
        Just todos' -> prompt todos'
handleCommand "show"         todos = do
        putStrLn ""
        putStrLn "Todo list:"
        mapM_ showTODOList (zip [0..] todos)
        prompt todos
handleCommand  "q"           todos = return ()
handleCommand  command       todos = do
    putStrLn ("Invalid command: `" ++ command ++ "`")
    prompt todos

delete :: Int -> [a] -> Maybe [a]
delete 0 (_:as) = Just as
delete n (a:as) = do
    as' <- delete (n - 1) as
    return (a:as')
delete _  []    = Nothing

main = do
    putStrLn "Commands:"
    putStrLn "+ <Item name> - Add new todo item"
    putStrLn "- <Item number> - Delete todo item by number"
    putStrLn "Show - show todo list"
    putStrLn "q - Quit"
    prompt []