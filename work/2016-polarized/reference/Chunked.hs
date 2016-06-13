{-# LANGUAGE     BangPatterns, NoMonomorphismRestriction #-}
module Chunked 
        ( Sources, Sinks
        , drainP
        , dup_ooo,   dup_ioi
        , project_i, project_o
        , funnel_i,  funnel_o
        , map_i,     map_o)
where
import qualified Generic                as G
import qualified Data.Vector.Unboxed    as U

type Sources i m e = G.Sources i m (U.Vector e)
type Sinks   i m e = G.Sinks   i m (U.Vector e)


-------------------------------------------------------------------------------
-- Operators where the chunked versions are defined by instantiating
-- the generic versions at a more specific type.

drainP  :: G.Range i => Sources i IO a -> Sinks i IO a -> IO ()
drainP     = G.drainP
{-# INLINE drainP #-}


dup_ooo :: (Ord i, Monad m)
        => Sinks i m a -> Sinks i m a -> Sinks i m a
dup_ooo = G.dup_ooo
{-# INLINE dup_ooo #-}


dup_ioi :: (Ord i, Monad m)
        => Sources i m a -> Sinks i m a -> Sources i m a
dup_ioi = G.dup_ioi
{-# INLINE dup_ioi #-}


project_i :: Eq i 
          => i -> Sources i m a -> Sources () m a
project_i = G.project_i 
{-# INLINE project_i #-}


project_o :: (Monad m, G.Range i) 
          => i -> Sinks i m a   -> Sinks () m a
project_o = G.project_o
{-# INLINE project_o #-}


funnel_i :: G.Range i => Sources i IO a -> IO (Sources () IO a)
funnel_i  = G.funnel_i
{-# INLINE funnel_i #-}


funnel_o :: G.Range i => i -> Sinks () IO a -> IO (Sinks i IO a)
funnel_o  = G.funnel_o
{-# INLINE funnel_o #-}


-------------------------------------------------------------------------------
-- Operators defined by applying a simple lifting to the generic version.

map_i   :: (U.Unbox a, U.Unbox b) 
        => (a -> b) -> Sources i m a -> Sources i m b
map_i f ss = G.map_i (U.map f) ss
{-# INLINE [5] map_i #-}


map_o   :: (U.Unbox a, U.Unbox b) 
        => (a -> b) -> Sinks i m b -> Sinks i m a
map_o f ss = G.map_o (U.map f) ss
{-# INLINE [5] map_o #-}

