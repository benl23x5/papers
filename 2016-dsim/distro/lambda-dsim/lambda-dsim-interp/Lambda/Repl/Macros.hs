module Lambda.Repl.Macros where
import Lambda.Core.Parser
import Lambda.Core.Lexer
import Lambda.Core.Exp
import Data.List


-- | Load macro definitions from the given file.
loadMacros :: String -> IO [(Macro, Exp)]
loadMacros fileName
 = do   -- Read the file.
        file    <- readFile fileName

        -- Macros are defined on lines that start with a # character.
        -- Treat everything else as a comment.
        let stripped
                = filter (isPrefixOf "#") $ lines file

        -- Parse all the definitions.
        case sequence $ map parseDef stripped of
         Left  err    -> error $ show err
         Right macros -> return macros


-- | Parse a macro definition 
parseDef :: String -> Either ErrorMacro (Macro, Exp)
parseDef line
 = case words line of 
        ('#' : name) : "=" : defStr
          -> case parseExp $ tokenize $ concat $ intersperse " " $ defStr of
                Just def -> Right (M name, def)
                Nothing  -> Left $ ErrorCannotParse (M name)
        _  -> Left $ ErrorBadLine


-- | Errors that can happen when we're parsing macro definitions.
data ErrorMacro
        = ErrorBadLine
        | ErrorCannotParse Macro
        deriving Show
