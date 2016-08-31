module Lambda.Core.Exp where


-------------------------------------------------------------------------------
-- | A variable name represented as a String.
data Var        
        = V String
        deriving (Show, Eq, Ord)


-- | A macro name represented as a String.
data Macro
        = M String
        deriving (Show, Eq, Ord)


-------------------------------------------------------------------------------
-- | Expressions abstract syntax.
data Exp
        -- | Variables.
        = XVar Var

        -- | Lambda abstraction with explicit substitution.
        | XAbs Subst Var Exp 

        -- | Function application.
        | XApp Exp Exp

        -- | A macro name.
        | XMacro Macro

        -- | A primitive operator.
        | XPrim  ExpPrim
        deriving (Show, Eq)


-- | Primitives.
data ExpPrim
        = PNat  Int
        | PBool Bool
        | PNot
        | PAdd
        | PIf
        | PEquals
        deriving (Show, Eq)


-- | Primitive operators and their names.
expPrimitives :: [(String, ExpPrim)]
expPrimitives
 =      [ ("not",       PNot)
        , ("true",      PBool True)
        , ("false",     PBool False)
        , ("add",       PAdd) 
        , ("if",        PIf)
        , ("equals",    PEquals) ]


-- | Check if an expression is a variable.
isXVar :: Exp -> Bool
isXVar xx
 = case xx of
        XVar _          -> True
        _               -> False


-- | Check if an expression is a macro name.
isXMacro :: Exp -> Bool
isXMacro xx
 = case xx of
        XMacro _        -> True
        _               -> False


-------------------------------------------------------------------------------
-- | A simultaneous substitution.
type Subst = [(Var, Exp)]


-- | Apply a simultaneous subsitution to the given expression.
--   Look 'ma, no alpha-conversion.
subst :: Subst -> Exp -> Exp
subst ss xx
 = case xx of
        XVar v
         -> case lookup v ss of
                Nothing -> xx
                Just x' -> x'

        XAbs ss' v x
         -> XAbs (mapSnd (subst ss) ss' ++ ss) v x

        XApp x1 x2
         -> XApp (subst ss x1) (subst ss x2)

        XMacro{} -> xx
        XPrim{}  -> xx


