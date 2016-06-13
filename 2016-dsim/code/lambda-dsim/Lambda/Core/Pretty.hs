module Lambda.Core.Pretty where
import Lambda.Core.Exp
import qualified Data.List      as List


-------------------------------------------------------------------------------
-- | Pretty print an expression.
prettyExp :: Exp -> String
prettyExp xx
 = case xx of
        -- Variables.
        XVar (V var)    
         -> var

        -- Abstractions.
        XAbs [] (V var) e  
         -> "\\" ++ var ++ ". " ++ prettyExp e

        XAbs ss (V var) e  
         -> prettySubst ss
                ++ " :- \\" ++ var ++ ". " ++ prettyExp e

        -- The expressions in an application are printed differently
        -- depending on if they are compound expressions.
        XApp e1 e2      
         -> prettyExpLeft e1 ++ " " ++ prettyExpRight e2

        -- Macros are printed with a leading '#'.
        XMacro (M str)  
         -> "#" ++ str

        -- Literal values.
        XPrim (PNat n)      -> show n
        XPrim (PBool True)  -> "true"
        XPrim (PBool False) -> "false"

        -- For primitives which are not literal values, 
        -- lookup their names in the primitives table.
        XPrim p
         -> case List.find (\t -> snd t == p) expPrimitives of
             Just (name, _) -> name
             Nothing        -> error $ "pretty: no name defined for " ++ show p


-- | Pretty print an expression on the left of an application.
prettyExpLeft :: Exp -> String
prettyExpLeft xx
 = case xx of
        XAbs{}          -> parens (prettyExp xx)
        _               -> prettyExp xx


-- | Pretty print an expression on the right of an application.
prettyExpRight :: Exp -> String
prettyExpRight xx
 = case xx of
        XAbs{}          -> parens (prettyExp xx)
        XApp{}          -> parens (prettyExp xx)
        _               -> prettyExp xx


-- | Pretty print a substitution.
prettySubst :: Subst -> String
prettySubst []  = ""
prettySubst ss
 = "[" 
 ++ (List.intercalate ", "
        [ v     ++ " = " ++ prettyExpRight x
        | (V v, x)     <- ss])
 ++ "]"


-- | Wrap a string in parenthesis.
parens :: String -> String
parens ss
 = "(" ++ ss ++ ")"


