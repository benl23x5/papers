{-# LANGUAGE TypeOperators #-}

-- | Closure construction and application.
module Data.Array.Closure
        ( ($:),  ($:^)
        , closure1, closure2, closure3)
where
import Data.Array.PArray.Closure
import Data.Array.PArray.Tuple 
import Data.Array.PArray.Base 
import qualified Prelude as P
import Prelude (($), Int)


-- | Closure application.
($:) :: (a :-> b) -> a -> b
($:) (Clo fv _fl env) x  
        = fv env x
infixl 1 $:


-- | Lifted closure application.
($:^) :: PArray (a :-> b) -> PArray a -> PArray b
PArray n (AClo _fv fl envs) $:^ PArray _ as 
        = PArray n (fl n envs as)
infixl 1 $:^


-- Closure1 -------------------------------------------------------------------
closure1 
        :: (a -> b) 
        -> (Int -> PArray a -> PArray b)
        -> (a :-> b)
closure1 fv fl
 = let  fl' n pdata
         = case fl n (PArray n pdata) of
                PArray _ pdata' -> pdata'

   in   Clo  (\_env -> fv)
             (\n _env -> fl' n)
             ()


-- Closure2 -------------------------------------------------------------------
closure2 
        :: (PR a, PR b, PR c)
        => (a -> b -> c)
        -> (Int -> PArray a -> PArray b -> PArray c)
        -> (a :-> b :-> c)

closure2 fv fl
 = let  fl' n pdata1 pdata2
         = case fl n (PArray n pdata1) (PArray n pdata2) of
                PArray _ pdata' -> pdata'

        fv_1 _ xa   = Clo  fv fl' xa
        fl_1 _ _ xs = AClo fv fl' xs
        
   in   Clo fv_1 fl_1 ()


-- Closure3 -------------------------------------------------------------------
closure3
        :: (PR a, PR b, PR c, PR d)
        => (a -> b -> c -> d)
        -> (Int -> PArray a -> PArray b -> PArray c -> PArray d)
        -> (a :-> b :-> c :-> d)

closure3 fv fl
 = let  fl' n pdata1 pdata2 pdata3
         = case fl n (PArray n pdata1) (PArray n pdata2) (PArray n pdata3) of
                PArray _ pdata' -> pdata'

        fv_1   _ xa = Clo   fv_2 fl_2 xa
        fl_1 _ _ xs = AClo  fv_2 fl_2 xs

        -----
        fv_2 xa yb   = Clo  fv_3 fl_3 (xa, yb)
        fl_2 _ xs ys = AClo fv_3 fl_3 (PTuple2 xs ys)

        -----
        fv_3 (xa, yb) zc           = fv xa yb zc
        fl_3 n (PTuple2 xs ys) zs  = fl' n xs ys zs

   in   Clo fv_1 fl_1 ()

