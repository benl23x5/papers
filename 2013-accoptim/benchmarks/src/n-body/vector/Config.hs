{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE TemplateHaskell #-}

module Config (

  Config, Solver(..),
  configSolver, configWindowSize, configShouldDrawTree,
  configRate, configBodyCount, configBodyMass, configTimeStep, configEpsilon,
  configStartDiscSize, configStartSpeed, configMaxSteps, configBenchmark,
  configDumpFinal,

  parseArgs,

) where

import Common.Type

import Data.Char
import Data.List
import Data.Label
import System.Exit
import System.Console.GetOpt
import qualified Criterion.Main                         as Criterion
import qualified Criterion.Config                       as Criterion


-- | Program configuration
--
data Solver = Naive | BarnsHut
  deriving (Enum, Bounded, Show)


data Config
  = Config
  {
    -- How to execute the simulation
    _configSolver               :: Solver

    -- How to present the output
  , _configWindowSize           :: Int
  , _configShouldDrawTree       :: Bool
  , _configRate                 :: Int

    -- System setup
  , _configBodyCount            :: Int
  , _configBodyMass             :: R
  , _configTimeStep             :: R
  , _configEpsilon              :: R

    -- Initial conditions
  , _configStartDiscSize        :: R
  , _configStartSpeed           :: R

    -- Terminating conditions
  , _configMaxSteps             :: Maybe Int
  , _configBenchmark            :: Bool
  , _configHelp                 :: Bool

    -- Dump final particle locations to file
  , _configDumpFinal            :: Maybe FilePath
  }
  deriving Show

$(mkLabels [''Config])


defaultConfig :: Config
defaultConfig = Config
  {
    _configSolver               = Naive         -- no barns-hut yet!

  , _configWindowSize           = 1000
  , _configShouldDrawTree       = False         -- no barns-hut yet!
  , _configRate                 = 30

  , _configBodyCount            = 200
  , _configBodyMass             = 100
  , _configTimeStep             = 1
  , _configEpsilon              = 50

  , _configStartDiscSize        = 350
  , _configStartSpeed           = 0.5

  , _configMaxSteps             = Nothing
  , _configBenchmark            = False
  , _configHelp                 = False

  , _configDumpFinal            = Nothing
  }


-- | The set of available command-line options
--
defaultOptions :: [OptDescr (Config -> Config)]
defaultOptions =
  [ Option  ['s'] ["solver"]
            (ReqArg (set configSolver . solver) "ALGORITHM")
            ("solver to use, one of: " ++ intercalate ", " (map show [minBound .. maxBound :: Solver]))

  , Option  [] ["size"]
            (ReqArg (set configWindowSize . read) "INT")
            (describe configWindowSize "visualisation size")

  , Option  [] ["framerate"]
            (ReqArg (set configRate . read) "INT")
            (describe configRate "visualisation frame rate")

  , Option  [] ["draw-tree"]
            (NoArg (set configShouldDrawTree True))
            "draw the Barns-Hut quad tree"

  , Option  ['n'] ["bodies"]
            (ReqArg (set configBodyCount . read) "INT")
            (describe configBodyCount "number of bodies in the simulation")

  , Option  [] ["mass"]
            (ReqArg (set configBodyMass . read) "FLOAT")
            (describe configBodyMass "mass of each body")

  , Option  [] ["timestep"]
            (ReqArg (set configTimeStep . read) "FLOAT")
            (describe configTimeStep "time step between simulation states")

  , Option  [] ["epsilon"]
            (ReqArg (set configEpsilon . read) "FLOAT")
            (describe configEpsilon "smoothing parameter")

  , Option  [] ["disc"]
            (ReqArg (set configStartDiscSize . read) "FLOAT")
            (describe configStartDiscSize "initial size of particle disc")

  , Option  [] ["speed"]
            (ReqArg (set configStartSpeed . read) "FLOAT")
            (describe configStartSpeed "initial rotation speed of the disc")

  , Option  [] ["max-steps"]
            (ReqArg (set configMaxSteps . read) "INT")
            (describe configMaxSteps "exit simulation after this many steps")

  , Option  [] ["benchmark"]
            (NoArg (set configBenchmark True))
            (describe configBenchmark "benchmark instead of displaying animation")

  , Option  [] ["dump-final"]
            (ReqArg (set configDumpFinal . Just) "FILE")
            "dump final body positions to file"

  , Option  ['h', '?'] ["help"]
            (NoArg (set configHelp True))
            "show this help message"
  ]
  where
    solver algorithm
      | a `elem` ["n",  "naive"]                        = Naive
      | a `elem` ["bh", "barnshut", "barns-hut"]        = BarnsHut
      | otherwise                                       = error $ "Unknown solver method: " ++ algorithm
      where
        a = map toLower algorithm

    describe f msg
      = msg ++ " (" ++ show (get f defaultConfig) ++ ")"


-- | Process the command line options
--

basicHeader :: String
basicHeader = unlines
  [ "vector-nbody (c) [2012..2013] The Accelerate Team"
  , ""
  , "Usage: vector-nbody [OPTIONS]"
  ]

fancyHeader :: Config -> String
fancyHeader _ = basicHeader


parseArgs :: [String] -> IO (Config, Criterion.Config, [String])
parseArgs argv =
  let
      helpMsg err = concat err
        ++ usageInfo basicHeader                    defaultOptions
        ++ usageInfo "\nGeneric criterion options:" Criterion.defaultOptions

  in case getOpt' Permute defaultOptions argv of
      (o,_,n,[])  -> do

        -- pass unrecognised options to criterion
        (cconf, rest)     <- Criterion.parseArgs Criterion.defaultConfig Criterion.defaultOptions n
        case foldr id defaultConfig o of
          conf | False <- get configHelp conf   -> putStrLn (fancyHeader conf) >> return (conf, cconf, rest)
          _                                     -> putStrLn (helpMsg [])       >> exitSuccess

      (_,_,_,err) -> error (helpMsg err)

