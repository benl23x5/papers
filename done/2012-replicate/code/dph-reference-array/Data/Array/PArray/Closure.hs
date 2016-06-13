{-# LANGUAGE TypeOperators, TypeFamilies, 
             ExistentialQuantification #-}

-- | PR instance for Closures.
module Data.Array.PArray.Closure
        ( (:->)  (..)
        , PData  (..)
        , PDatas (..))
where
import Data.Array.PArray.Unit
import Data.Array.PArray.Base


-- | Closures.
--   Holds the unlifted and lifted version of the function,
--   and any captured environment.
data (a :-> b) 
 = forall env. PR env 
 => Clo  (env -> a -> b) 
         (Int -> PData env -> PData a -> PData b) 
         env
infixr 0 :->


-- | Arrays of closures with a linear index space.
data instance PData (a :-> b)
 =  forall env. PR env
 => AClo  (env -> a -> b)
          (Int -> PData env -> PData a -> PData b)
          (PData env)


-- | Arrays of closures with a 2D index space.
data instance PDatas (a :-> b)
 =  forall env. PR env
 => AClos (env -> a -> b)
          (Int -> PData env -> PData a -> PData b)
          (PDatas env)


instance PR (a :-> b) where
 -- The function in an empty closure can never be applied,
 -- because there is no environment to go with it.
 emptyPR 
  = let die :: forall a. a
        die = error "emptyPR[:->] no function to apply"
    in  AClo die die (emptyPR :: PData ())

 lengthPR (AClo _ _ env)
  = lengthPR env

 replicatePR n   (Clo fv fl env)
  = AClo fv fl (replicatePR n env)

 replicatesPR ns (AClo fv fl envs)
  = AClo fv fl (replicatesPR ns envs)

 indexPR   (AClo fv fl envs) i
  = Clo  fv fl (indexPR envs i)

 indexvsPR (AClos fv fl envs) vsegd segixs
  = AClo fv fl (indexvsPR envs vsegd segixs)

 extractPR   (AClo fv fl env) start len
  = AClo fv fl (extractPR env start len)

 extractvsPR (AClos fv fl envs) vsegd
  = AClo fv fl (extractvsPR envs vsegd)

 packPR (AClo fv fl envs) flags
  = AClo fv fl (packPR envs flags)

 -- The functions in an empty collection of closures can never
 -- be applied, because there are no environments to go with them.
 emptydPR 
  = let die :: forall a. a
        die = error "emptydPR[:->] no function to apply"
    in  AClos die die (emptydPR :: PDatas ())

 singletondPR (AClo fv fl env)
  = AClos fv fl (singletondPR env)

 lengthdPR (AClos _ _ envs)
  = lengthdPR envs

 indexdPR  (AClos fv fl envs) i
  = AClo fv fl (indexdPR envs i)

 packdPR   (AClos fv fl envs) flags
  = AClos fv fl (packdPR envs flags)

 -- These operations aren't supported by this implementation.
 combinePR  = dieHetroFunctions
 fromListPR = dieHetroFunctions
 appendPR   = dieHetroFunctions
 appenddPR  = dieHetroFunctions
 concatdPR  = dieHetroFunctions

dieHetroFunctions :: a
dieHetroFunctions
 = error $ unlines 
         [ "Unsupported Array Operation"
         , "Not creating an array containing possibly different functions."
         , "This isn't really a data parallel operation, and making it"
         , "efficient is more effort than we care for right now."
         , "For discussion of how this would work refer to:"
         , ""
         , "  Higher Order Nested Data Parallelism"
         , "  Roman Leshchinskiy"
         , "  PhD Thesis, 2006" ]

