import Data.Char

-- Quiz 6
q6a = do x <- getLine
         putStrLn (filter isDigit x)
         q6a
         
-- Choices:        
    
q61 = getLine 
      >>= putStrLn . filter isDigit 
      >> q61
q62 = getLine 
      >>= \x -> putStrLn (filter isDigit x) 
      >>= \_ -> q62
-- choice 3 has type-error
-- choice 4 has type-error
-- choice 5 has type-error
q66 = do x <- getLine; 
         putStrLn . filter isDigit $ x; 
         q66
q67 = do getLine >>= \x -> putStrLn (filter isDigit x); 
         q67