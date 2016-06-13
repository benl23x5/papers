
module Main where
import Lambda.Repl.Macros
import Lambda.Repl
import qualified System.Console.Haskeline       as H


-- | Entry point for the interpreter REPL.
main :: IO ()
main 
 = do   -- Load macros from a file in the working directory.
        macros  <- loadMacros "Prelude.macros"

        -- Print hello message and help.
        putStrLn hello

        -- Start the REPL.
        H.runInputT H.defaultSettings (repl macros)


hello :: String
hello
 = unlines
 [ "λ-don't-substitute-into-me"
 , ""
 , " Try these examples:"
 , "   (\\x. \\y. add x y z) (succ y) five"
 , "   #fac #three"
 , "   (#fac #three) s z" ]