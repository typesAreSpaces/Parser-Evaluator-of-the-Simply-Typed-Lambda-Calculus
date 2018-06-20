import Syntax
import TypingContext
import DeBruijnIndex
import Typing
import Parser
import Evaluator
import System.Environment

main = do
  file <- getArgs
  line <- readFile $ head file
  putStrLn "---Term:---"
  if ((parseTerm . tokenize) line == Nothing)
    then do
      putStrLn "TLBN: parse error"
    else do
      parsedProgram  <- case ((parseTerm . tokenize) line) of 
        Just x -> return x
        Nothing -> error "parse error"
      putStrLn $ show parsedProgram
      putStrLn "---Type:---"
      typeOfProgram <- (return . typeOf []) parsedProgram
      if (typeOf [] parsedProgram /= Nothing) 
        then do 
              putStrLn $ showType typeOfProgram
              putStrLn "---Normal Form:---"
              normalFormOfProgram <- (return . evalTerm) parsedProgram 
              putStrLn $ show2 normalFormOfProgram
        else 
            putStrLn $ showType typeOfProgram