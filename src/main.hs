{-
 - TODO
 - 
 - Register handling
 - Arrays
 - Void
 - Break / Continue
 - Floats
 - 
 -}

import Data.List
import System.Environment

import Tokenize
import Parse
import Rtl
import Asm

--- COMPILE

compile :: String -> Asm
compile = (uncurry toAsm) . toRtl . parse . tokenize

--- MAIN

main :: IO ()
main = do
    args <- getArgs
    if (length args) == 2
        then do
            file <- readFile $ (args !! 0)
            writeFile (args !! 1) $ intercalate "\n" $ compile $ file
        else
            putStrLn $ intercalate "\n" $ compile $ (head args)