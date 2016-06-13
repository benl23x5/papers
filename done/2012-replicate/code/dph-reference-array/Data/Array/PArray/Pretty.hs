{-# LANGUAGE StandaloneDeriving, ParallelListComp,
             FlexibleInstances,  FlexibleContexts,
             UndecidableInstances #-}

-- | Pretty printing for parallel arrays.
module Data.Array.PArray.Pretty
        ( module Text.PrettyPrint.Leijen
        , Show   (..)
        , Logical(..)
        , physical)
where
import Data.Array.PArray.Base
import Text.PrettyPrint.Leijen
import qualified Data.Vector.Unboxed    as U
import qualified Data.Vector            as V


-- Show -----------------------------------------------------------------------
deriving instance Show (PData  a) => Show (PArray a)

deriving instance Show (PData  Int)
deriving instance Show (PDatas Int)
deriving instance Show (PData  Char)
deriving instance Show (PDatas Char)

deriving instance Show VSegd
deriving instance Show SSegd
deriving instance Show Segd
deriving instance Show (PDatas a) => Show (PData  (PArray a))
deriving instance Show (PDatas a) => Show (PDatas (PArray a))


-- Physical -------------------------------------------------------------------
-- | Pretty print the physical representation of an array.
physical :: Pretty a => a -> Doc
physical = pretty


-- Needs UndecidableInstances
instance Pretty (PData a) => Pretty (PArray a) where
 pretty (PArray n xs)
  = vcat [ text "PArray" <+> pretty n
         , indent 1 (pretty xs) ]


instance Pretty (PData Int) where
 pretty (PInt xs) 
  =   text "PInt" 
  <+> parens (text $ show $ U.toList xs)


instance Pretty (PDatas Int) where
 pretty (PInts xss) 
  =   text "PInts" 
  <+> (align $ vcat 
        [ int i <> text ": " <> (text $ show $ U.toList xs)
                | i  <- [0..]
                | xs <- V.toList xss ])


instance Pretty (PData Char) where
 pretty (PChar xs) 
  =   text "PChar" 
  <+> parens (text $ show $ U.toList xs)


instance Pretty (PDatas Char) where
 pretty (PChars xss) 
  =   text "PChars" 
  <+> (align $ vcat 
        [ int i <> text ": " <> (text $ show $ U.toList xs)
                | i  <- [0..]
                | xs <- V.toList xss ])
                

instance Pretty VSegd where
 pretty (VSegd segmap ssegd)
  = vcat [ text "VSegd  segmap:" <+> (text $ show $ U.toList segmap)
         , pretty ssegd ]


instance Pretty SSegd where
 pretty (SSegd  sources starts segd)
  = vcat [ text "SSegd sources:" <+> (text $ show $ U.toList sources)
         , text "       starts:" <+> (text $ show $ U.toList starts) 
         , pretty segd ]


instance Pretty Segd where
 pretty (Segd lengths indices)
  = vcat [ text " Segd lengths:" <+> (text $ show $ U.toList lengths)
         , text "      indices:" <+> (text $ show $ U.toList indices) ]


instance Pretty (PDatas a) => Pretty (PData (PArray a)) where
 pretty (PNested vsegd pdatas) 
  = vcat [ text "PNested"
         , indent 1 $ vcat [ pretty vsegd
                           , pretty pdatas ]]

instance Pretty (PDatas a) => Pretty (PDatas (PArray a)) where
 pretty (PNesteds xs)
  = vcat [ text "PNesteds"
         , indent 1 $ vcat 
                [ int i <> text ":" 
                <> indent 1 (pretty x)
                         | i   <- [0..]
                         | x   <- V.toList xs ]]


-- Logical --------------------------------------------------------------------
class Logical a where
 -- | Pretty print the logical value of an array.
 logical        :: a       -> Doc

 -- | Pretty print the logical value of some array data.
 logicalPData   :: PData a -> Doc


instance Logical Int where
 logical i
        = int i

 logicalPData (PInt is) 
        = text $ show $ U.toList is


instance Logical Char where
 logical i
        = char i

 logicalPData (PChar is) 
        = text $ show $ U.toList is


-- Needs UndecidableInstances
instance (Logical a, PR (PArray a)) => Logical (PArray a) where
 logical (PArray _ pdata)
        = logicalPData pdata
 
 logicalPData pdata
        | lengthPR pdata == 0
        = text "[]"

        | otherwise
        = brackets $ hsep $ punctuate comma
        $ map   (\i -> logical (indexPR pdata i))
                [0 .. lengthPR pdata - 1]

