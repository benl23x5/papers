module Lambda.Core.Parser where
import Lambda.Core.Exp 
import Lambda.Core.Lexer


-- | Tokenise a string and then try to parse it.
loadExp  :: String  -> Maybe Exp
loadExp = parseExp . tokenize


-------------------------------------------------------------------------------
-- | Parse a lambda expression, 
--   or `Nothing` if there was a parse error.
parseExp :: [Token] -> Maybe Exp
parseExp ks
 = case parseExpSome ks of
        -- We completely parsed the tokens.
        Just (xx, [])   -> Just xx

        -- We parsed some prefix of the tokens,
        -- but there was junk at the end that isn't part of the expression.
        Just (_, _)     -> Nothing

        -- Some other parse error.
        Nothing         -> Nothing
        

-- | Try to parse some tokens as an expression.
parseExpSome :: [Token] -> Maybe (Exp, [Token])
parseExpSome ks
 = case parseExpBits ks of
        (x : xs, ks')   -> Just (buildExpApp x xs, ks')
        ([],     _)     -> Nothing

 
-- | Parse a sequence of non-application expressions.
parseExpBits :: [Token] -> ([Exp], [Token])
parseExpBits ks
 = case parseExpBit ks of
        Nothing         
         -> ([], ks)

        Just (e, ks')   
         -> let (es, rest)      = parseExpBits ks'
            in  (e : es, rest)


-- | Parse some non-application expressions.
parseExpBit :: [Token] -> Maybe (Exp, [Token])

parseExpBit (KBra : ks)
        | Just (e, KKet : ks')  <- parseExpSome ks
        = Just (e, ks')

parseExpBit (KVar str : ks)        
        = Just (XVar (V str), ks)

parseExpBit (KLam : KVar str : KDot : ks)
        | Just (e, ks')         <- parseExpSome ks
        = Just (XAbs [] (V str) e, ks')

parseExpBit (KMacro str : ks)      
        = Just (XMacro (M str), ks)

parseExpBit (KInt n : ks)
        = Just (XPrim (PNat n), ks)

parseExpBit (KPrim str : ks)
 = Just (XPrim xPrim, ks)
 where  xPrim   
         = case lookup str expPrimitives of
                Just p  -> p
                Nothing -> error $ "parseExpBit: no name for " ++ show str
parseExpBit _
        = Nothing


-- | Create some left-associated applications from a
--   list of expressions.
buildExpApp :: Exp -> [Exp] -> Exp
buildExpApp x0 xx0
 = buildExpApp' $ reverse (x0 : xx0)
 where 
       buildExpApp' xx
        = case xx of
           []           -> error "buildApp': list should be non-empty"
           x : []       -> x
           x : xs       -> XApp (buildExpApp' xs) x

        