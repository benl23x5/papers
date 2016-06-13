
--
-- Generators for various distributions of particle positions
--
module Random.Position
  where

import Common.Type

import Control.Monad.ST                         ( ST )
import System.Random.MWC                        ( GenST, uniformR )


-- | A uniform distribution of points between some minimum and maximum bounds
--
{-# INLINE uniform #-}
uniform :: R -> R -> Int -> GenST s -> ST s Position
uniform pointMin pointMax _ix
  = uniformR ((pointMin, pointMin, pointMin), (pointMax, pointMax, pointMax))


-- | Points distributed as a disc
--
{-# INLINE disc #-}
disc :: Position -> R -> Int -> GenST s -> ST s Position
disc (originX, originY, originZ) radiusMax _ix gen
  = do  radius          <- uniformR (0,radiusMax) gen
        theta           <- uniformR (0, pi)       gen
        phi             <- uniformR (0, 2*pi)     gen

        return ( originX + radius * sin theta * cos phi
               , originY + radius * sin theta * sin phi
               , originZ + radius * cos theta )


-- | A point cloud with areas of high and low density
--
{-# INLINE cloud #-}
cloud :: Int -> GenST s -> ST s Position
cloud ix gen
  = case ix `mod` 5 of
      0 -> disc (250,250,250) 200 ix gen
      1 -> disc (100,100,100) 80  ix gen
      2 -> disc (150,300,300) 30  ix gen
      3 -> disc (500,120,120) 30  ix gen
      _ -> disc (300,200,200) 150 ix gen

