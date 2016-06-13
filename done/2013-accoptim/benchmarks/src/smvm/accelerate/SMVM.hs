{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMVM where

import Data.Array.Accelerate            as A


-- Sparse-matrix vector multiplication
-- -----------------------------------

type SparseVector a = (Segments Int32, Vector a)
type SparseMatrix a = (Segments Int32, SparseVector a)


smvm :: forall a. (Elt a, IsFloating a) => Acc (SparseMatrix a) -> Acc (Vector a) -> Acc (Vector a)
smvm smat vec
  = let (segd, svec)    = unlift smat           :: (Acc (Segments Int32), Acc (SparseVector a))
        (inds, vals)    = unlift svec

        vecVals         = backpermute (shape inds) (\i -> index1 (A.fromIntegral $ inds ! i)) vec
        products        = A.zipWith (*) vecVals vals
    in
    foldSeg (+) 0 products segd

