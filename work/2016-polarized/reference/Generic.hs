{-# LANGUAGE BangPatterns, NoMonomorphismRestriction #-}
module Generic where
import System.IO
import Control.Concurrent
import Control.Exception (bracket_)
import Data.IORef
import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- Section 2: Streams and Flows.

-- | Source end point.
data Sources i m e 
   = Sources
   { iarity :: i
   , pull   :: i -> (e -> m ()) -> m () -> m () }

-- Sink end point.
data Sinks   i m e 
   = Sinks   
   { oarity :: i
   , push   :: i -> e -> m ()
   , eject  :: i -> m () }


class (Eq a, Ord a) => Range a where
 zero   :: a
 next   :: a -> a
 range  :: a -> [a]

instance Range () where
 zero    = ()
 next _  = ()
 range x = [()]

instance Range Int where
 zero    = 0
 next  x = x + 1
 range x = [0 .. x - 1]


-------------------------------------------------------------------------------
-- Section 2.1: Sourcing, Sinking and Draining.

-- | Create sources from some named files.
sourceFs :: [FilePath] -> IO (Sources Int IO Char)
sourceFs names 
 = do   hs <- mapM (\n -> openFile n ReadMode) names
        let pulls_sourceFs i ieat ieject
             = do let h = hs !! i
                  eof <- hIsEOF h
                  if eof then hClose   h >> ieject
                         else hGetChar h >>= ieat
            {-# INLINE pulls_sourceFs #-}

        return (Sources (length names) pulls_sourceFs)
{-# NOINLINE sourceFs #-}


-- | Create sinks to some named files.
sinkFs  :: [FilePath] -> IO (Sinks Int IO Char)
sinkFs names
 = do   hs <- mapM (\n -> openFile n WriteMode) names
        let pushs_sinkFs  i e = hPutChar (hs !! i) e
            {-# INLINE pushs_sinkFs #-}

        let ejects_sinkFs i   = hClose   (hs !! i)
            {-# INLINE ejects_sinkFs #-}
        return (Sinks (length names) pushs_sinkFs ejects_sinkFs)
{-# NOINLINE sinkFs #-}


-- | Drain elements from a source to a sink.
drainP :: Range i => Sources i IO a -> Sinks i IO a -> IO ()
drainP (Sources i1 ipull) (Sinks i2 opush oeject)
 = do mvs <- mapM makeDrainer (range (min i1 i2))
      mapM_ readMVar mvs
 where
  makeDrainer i
   = do mv <- newEmptyMVar
        forkFinally (newIORef True >>= drainStream i)
                    (\_ -> putMVar mv ())
        return mv

  drainStream i loop
   = let eats v = opush i v
         ejects = oeject i >> writeIORef loop False
     in  while (readIORef loop) (ipull i eats ejects)

  while p v
   = do b <- p
        if b then v >> while p v
             else return ()
{-# NOINLINE drainP #-}

-------------------------------------------------------------------------------
-- | Section 2.2: Stateful streams, branching and buffering.
dup_ooo :: (Ord i, Monad m)
        => Sinks i m a -> Sinks i m a -> Sinks i m a
dup_ooo (Sinks n1 push1 eject1) (Sinks n2 push2 eject2)
 = let  pushs_dup_ooo  i x = push1 i x >> push2 i x
        {-# INLINE pushs_dup_ooo #-}

        ejects_dup_ooo i   = eject1 i  >> eject2 i
        {-# INLINE ejects_dup_ooo #-}

   in  Sinks (min n1 n2) pushs_dup_ooo ejects_dup_ooo


dup_ioi :: (Ord i, Monad m)
        => Sources i m a -> Sinks i m a -> Sources i m a
dup_ioi (Sources n1 pull1) (Sinks n2 push2 eject2)
 = let  pull3_dup_ioi i eat3 eject3
         = pull1 i eat1_dup_ioi eject1_dup_ioi
         where  eat1_dup_ioi   x = eat3 x >> push2  i x
                {-# INLINE eat1_dup_ioi #-}

                eject1_dup_ioi   = eject3 >> eject2 i
                {-# INLINE eject1_dup_ioi #-}
        {-# INLINE pull3_dup_ioi #-}

   in  Sources (min n1 n2) pull3_dup_ioi
{-# INLINE dup_ioi #-}


-------------------------------------------------------------------------------
-- Section 2.4: Mapping

-- | Like `map_i`, but the worker function is also given the stream index.
map_i   :: (a -> b) -> Sources i m a -> Sources i m b
map_i f (Sources n pullsA)
 = Sources n pullsB_map_i
 where  pullsB_map_i i eatB ejectB
         = pullsA i eatA_map_i ejectA_map_i
         where  eatA_map_i v = eatB (f v)
                {-# INLINE [3] eatA_map_i #-}

                ejectA_map_i = ejectB
                {-# INLINE [3] ejectA_map_i #-}
        {-# INLINE [3] pullsB_map_i #-}
{-# INLINE [4] map_i #-}


-- | Like `map_o`, but the worker function is also given the stream index.
map_o  :: (a -> b) -> Sinks i m b -> Sinks i m a
map_o f (Sinks n pushB ejectB)
 = Sinks n pushA_map_o ejectA_map_o
 where  pushA_map_o  i a  = pushB  i (f a)
        {-# INLINE [3] pushA_map_o #-}

        ejectA_map_o i    = ejectB i
        {-# INLINE [3] ejectA_map_o #-}
{-# INLINE [4] map_o #-}


-------------------------------------------------------------------------------
-- Section 2.5: Folding

folds_iii
        :: (Ord i, Monad m)
        => (b -> a -> b) -> b
        -> Sources i m Int -> Sources i m a 
        -> Sources i m b

folds_iii f z (Sources nL pullLen) (Sources nX pullX)
 = Sources (min nL nX) pull_folds_iii
 where  
        pull_folds_iii i eat eject
         = pullLen i eat_len_folds_iii eject_len_folds_iii
         where 
               eat_len_folds_iii len = loop_folds len z
               {-# INLINE eat_len_folds_iii #-}

               eject_len_folds_iii   = eject
               {-# INLINE eject_len_folds_iii #-}
                   
               loop_folds !c !acc
                | c == 0    = eat acc
                | otherwise = pullX i eat_x eject_x
                where eat_x x = loop_folds (c - 1) (f acc x)
                      eject_x = eject
               {-# INLINE loop_folds #-}
        {-# INLINE pull_folds_iii #-}
{-# INLINE folds_iii #-}


-- | Non-segmented fold.
-- Performs fold for each part of bundle, then merges at the end.
fold_o  :: Range i
        => (a -> a -> a)  -- ^ fold function
        -> a              -- ^ seed
        -> i              -- ^ arity: number of sinks to return
        -> IO (Sinks i IO a, IORef a)
fold_o f z arity1
 = do -- Construct an IORef for each sink
      refs <- mapM (\_ -> newIORef z) (range arity1)
      let refmap = Map.fromList (range arity1 `zip` refs)

      -- Push just updates each local IORef at each stage
      -- No synchronisation is required here because the index is local
      let push' i e
           = do let ref = refmap Map.! i
                acc <- readIORef ref
                writeIORef ref (f acc e)
          {-# INLINE push' #-}

      -- We need an IORef for the final merged value
      val  <- newIORef z

      -- Eject merges the local IORef into the final ref
      -- This needs to be atomic, because multiple ejects could be happening
      -- and modifying the same final value.
      let eject' i
           = do let ref = refmap Map.! i
                acc <- readIORef ref
                let update v = (f v acc, ())
                atomicModifyIORef' val update
          {-# INLINE eject' #-}

      return (Sinks arity1 push' eject', val)
{-# INLINE fold_o #-}


-------------------------------------------------------------------------------
-- Section 2.6: Stream projection, funneling and fattening.

-- | Project out a single stream in a flow.
project_i :: Eq i 
          => i -> Sources i m a -> Sources () m a
project_i i (Sources _ pull)
 = Sources () pull_project_i'
 where  pull_project_i' () eat eject = pull i eat eject
        {-# INLINE pull_project_i' #-}
{-# INLINE project_i #-}


-- | Project out a single sink in a flow.
project_o :: (Monad m, Range i) 
          => i -> Sinks i m a   -> Sinks () m a
project_o i (Sinks n push eject)
 = Sinks () push_project_o' eject_project_o'
 where  push_project_o'  () x
         = push i x
        {-# INLINE push_project_o' #-}

        eject_project_o' ()
         = mapM_ eject (range n)
        {-# INLINE eject_project_o' #-}
{-# INLINE project_o #-}


-- | Funnel all source streams into a single one.
funnel_i :: Range i => Sources i IO a -> IO (Sources () IO a)
funnel_i (Sources n pull)
 = do   refIx   <- newIORef zero

        let pull_funnel_i' () eat' eject'
              = do 
                   -- Index of the stream we're currently pulling from.
                   ix    <- readIORef refIx

                   -- Signal whether the current stream is finished.
                   refB  <- newIORef  False

                   let eject_funnel_i' 
                        -- All argument streams are finished, so eject the result.
                        | ix >= n   = eject'

                        -- The current argument stream is finished.
                        | otherwise = do writeIORef refIx (next ix)
                                         writeIORef refB True
                       {-# INLINE eject_funnel_i' #-}

                   -- Try to pull from one of the argument streams.
                   pull ix eat' eject_funnel_i'

                   -- If the current argument stream is finished we want
                   -- to go to the next one.
                   b  <- readIORef refB
                   if b then pull_funnel_i' () eat' eject'
                        else return ()
            {-# INLINE pull_funnel_i' #-}

        return (Sources () pull_funnel_i')
{-# INLINE funnel_i #-}


-- | Non-deterministically funnel all sink streams into a single one.
funnel_o :: Range i => i -> Sinks () IO a -> IO (Sinks i IO a)
funnel_o arity1 (Sinks _ push1 eject1)
 = do lock <- newQSem 1
      fins <- newIORef zero

      let push' _ e
           = bracket_ (waitQSem lock) (signalQSem lock)
           $ push1 () e
          {-# INLINE push' #-}

      let eject' _
           = do fin <- atomicModifyIORef' fins (\o -> (next o, next o))
                case fin == arity1 of
                 True  -> eject1 ()
                 False -> return ()
          {-# INLINE eject' #-}

      return (Sinks arity1 push' eject')
{-# INLINE funnel_o #-}


