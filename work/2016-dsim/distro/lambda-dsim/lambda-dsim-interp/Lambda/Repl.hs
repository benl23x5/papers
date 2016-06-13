module Lambda.Repl where
import Lambda.Core.Lexer
import Lambda.Core.Parser
import Lambda.Core.Eval
import Lambda.Core.Pretty
import Lambda.Core.Exp
import qualified Control.Monad.IO.Class         as C
import qualified System.Console.Haskeline       as H


-- | The Read-Eval-Print-Loop.
--
--   * The 'InputT IO ()' means this computation gets some input from the
--     console, as well as performing some IO actions.
--
repl :: [(Macro, Exp)] -> H.InputT IO ()
repl macros
 = loop
 where
  loop = do
        -- Get a line from the console, using the given prompt.
        minput <- H.getInputLine "> "
        case minput of

         -- User closed stdin, so we're done.
         Nothing        -> return ()

         -- Explicit quit command.
         Just ":quit"   -> return ()

         -- If no particular command has been specified then evaluate
         -- the expression.
         Just str     
          -> do 
                _ <- C.liftIO
                  $  parseIO str
                  $  \x -> reduceIO macros x
                  $  \_ -> return (Just ())

                loop


-- | Try to parse an expression,
--   printing an error message if there is one.
parseIO :: String                       -- ^ String to parse.
        -> (Exp -> IO (Maybe b))        -- ^ Continue with parsed expression.
        -> IO (Maybe b)

parseIO str continue
 = case tokenize str of
        -- No tokens. Input was empty.
        [] 
         ->     return Nothing

        tokens 
         -> case parseExp tokens of
                Nothing
                 -> do  putStrLn $ "Parse Error\n"
                        return Nothing
                Just xx
                 ->     continue xx


-- | Reduce an expression,
--   printing each intermediate step in the reduction.
reduceIO
        :: [(Macro, Exp)]               -- ^ Macros.
        -> Exp                          -- ^ Expression to check.
        -> (Exp -> IO (Maybe b))        -- ^ Continue with normal form.
        -> IO (Maybe b)

reduceIO macros xx0 continue
 = loop (0 :: Int) xx0
 where
  loop count xx
   = case step macros xx of
        StepNone
         | count == 0
         -> do  putStrLn $ prettyExp xx
                putStr "\n"
                continue xx

         | otherwise        
         -> do  putStrLn $ "[" ++ show count ++ " steps]"
                putStr "\n"
                continue xx

        StepSome xx'
         -> do  putStrLn $ prettyExp xx'

                -- If the evaluator alleges the expression as stepped, 
                -- but it's still the same as before then we've diverged.
                -- Bail out now to avoid running forever.
                if xx == xx'
                 then do putStrLn "...\n"
                         continue xx

                 else loop (count + 1) xx'

        StepError err
         -> do  -- Some runtime error (crash).
                -- This won't happen with well typed expressions.
                putStrLn $ "ERROR: " ++ show err
                putStr "\n"                                               
                return Nothing

