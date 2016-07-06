{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GADTSyntax #-}

module Datum.Schema
where

import Prelude                          hiding (last, map, read, length, filter)
import qualified Prelude                as P
import qualified GHC.Exts               as P
import qualified System.Random          as R
import qualified Data.List              as L
import qualified Data.List.Split        as L
import qualified Data.Map.Strict        as M
import qualified Data.Dynamic           as T
import qualified Data.Maybe             as M
import Data.Decimal
import Control.Monad
import Control.Monad.State

rand = L.unfoldr (Just . R.random) (R.mkStdGen 0) :: [Int]
(|>) = flip ($)

type Name = String

data Dim where
 Dim :: { dimName   :: Name                 -- ^ name: short name of dimension
        , size      :: Size                 -- ^ size: expansion Size applied to the parent dimension
        , children  :: M.Map Name Dim       -- ^ children: child dimensions
        , columns   :: M.Map Name Column    -- ^ columns: all columns that share the shape of this dimension
        } -> Dim                            -- ^ resulting dimension (parent x size)
        deriving (Eq)


instance Show Dim where
    show Dim { dimName=n, size=s, children=c } 
        =  (n :: String) ++ show s ++ "\n" 
        ++ (L.intercalate "-" $ P.map dimName $ M.elems c)

dim  = Dim { dimName="", size=One, children=M.empty, columns=M.empty }
root = dim { dimName="^", size=One }


data Size where
    -- | Singleton value
    One     :: Size

    -- | Fixed number of items per parent value (regular)
    Fixed   :: Int          -- ^ size: number of children for every parent
            -> Size

    -- | Variable number of items per parent value (ragged)
    Ragged  :: [Int]        -- ^ size: number of children, per parent
            -> Size

    -- | Product of parent dimensions
    Product :: Int          -- ^ generations: number of ancestors to work within
            -> Size
    deriving (Eq)


instance Show Size where
    show One            = []
    show (Fixed l)      = "(" ++ show l ++ ")"
    show (Ragged ls)    = show (take 5 ls)              -- * BL: Why take 5 here?
    show (Product g)    = "^" ++ show g


data Loc where
 Loc :: { targDim   :: Dim
        , targCol   :: Maybe Column
        , siblings  :: M.Map Name Dim
        , parents   :: [(M.Map Name Dim, Dim)]
        } -> Loc
        deriving (Eq)


instance Show Loc where
    show Loc { targDim=d, parents=ps } 
        =  L.intercalate "." (P.map dimName $ L.reverse $ P.map snd ps) 
        ++ "." ++ show d

loc = Loc { targDim=root, targCol=Nothing, siblings=M.empty, parents=[] }


data Column 
        = Column
        { colName   :: Name
        , values    :: T.Dynamic
        , typ       :: String }

instance Show Column where
    show (Column { colName=n }) = n

instance Eq Column where
    (Column { colName=a }) == (Column { colName=b }) = a == b

col = Column { colName="", values=T.toDyn "", typ=""}



addDim    :: Dim -> State Loc ()
addDim child = do
    Loc { targDim=dim@(Dim { children=cs }), siblings=ss, parents=ps } <- get
    put Loc
        { targDim   = child
        , targCol   = Nothing
        , siblings  = cs
        , parents   = (ss, dim):ps
        }


-- | Pushes the target column into the dimension
clearTargCol :: State Loc ()
clearTargCol = do
    l@Loc { targDim=d@(Dim { columns=cs }), targCol=c} <- get
    case c of
        Just c' ->
            put l
            { targDim   = d { columns=M.insert (colName c') c' cs }
            , targCol   = Nothing
            }
    return ()

addCol    :: Column -> State Loc ()
addCol col@(Column { colName=n }) = do
    clearTargCol
    loc@Loc { targDim=dim@(Dim { columns=cs }) } <- get
    put loc
        { targDim=dim { columns=M.insert n col cs }
        , targCol=Just col
        }

moveCol :: Name
        -> State Loc (Maybe Column)
moveCol n = do
    l@(Loc { targDim=(Dim { columns=cs }) }) <- get
    let c = M.lookup n cs
    put l { targCol = c }
    return c

isRoot :: State Loc Bool
isRoot = do
    Loc { targDim=d } <- get
    return (dimName d == "^") -- TODO: (d == root) not working?

moveRoot :: State Loc ()
moveRoot = do
    r <- isRoot
    unless r $ do
        _ <- moveUp 1
        moveRoot

moveUp  :: Int -> State Loc Bool
moveUp 0        = return True
moveUp level    = do
    l <- get
    case l of
        Loc { targDim=c, parents=(pss,p):ps } -> do
            put loc
                { targDim   = p { children=M.insert (dimName c) c (children p)}
                , siblings  = pss
                , parents   = ps
                }
            moveUp (level-1)
        Loc {} -> return False

moveDown :: Name -> State Loc Bool
moveDown name = do
    Loc { parents=ps, siblings=ss, targDim=(Dim { children=cs }) } <- get
    case M.lookup name cs of
        Just child -> do
            put Loc
                { targDim   = child
                , targCol   = Nothing
                , siblings  = M.delete name cs
                , parents   = (ss, dim):ps
                }
            return True
        Nothing -> return False

moveSib :: Name -> State Loc Bool
moveSib name = do
    loc@(Loc { siblings=ss, targDim=dim@(Dim { dimName=n }) }) <- get
    case M.lookup name ss of
        Just sibling -> do
            put loc
                { targDim   = sibling
                , siblings  = ss
                              |> M.insert n dim
                              |> M.delete name
                }
            return True
        Nothing -> return False

--  ^:
--  |
--  `-- manager(4):
--      |
--      +-- fund[2,3,4,5]: code
--      |   |
--      |   +-- date(10): date, price
--      |   |
--      |   +-- fund(14): correl
--      |   |
--      |   `-- manager(4):
--      |       |
--      |       `-- fund[2,3,4,5]: correl
--      |
--      `-- name[3,3,3,3]: value

ls =
  -- Date,ManagerCode,FundCode,OwnerCode,Units,Price
    [ "0,ABC,L,A,1000,1.23"
    , "0,ABC,L,B,500,1.23"
    , "0,ABC,M,B,2000,2.56"
    , "0,ABC,M,C,500,2.56"
    , "0,ABC,M,D,900,2.56"
    , "0,ABC,N,A,50,0.96"
    , "0,DEF,L,A,1000,4.34"
    , "0,GHI,L,B,500,4.34"
    , "0,GHI,M,B,2000,2.98"
    , "0,GHI,M,C,500,2.98"
    , "0,GHI,M,D,900,2.98"
    , "1,ABC,L,A,900,1.21"
    , "1,ABC,L,B,500,1.21"
    , "1,ABC,M,B,2000,2.59"
    , "1,ABC,M,C,500,2.59"
    , "1,ABC,M,D,900,2.59"
    , "1,ABC,N,A,50,0.97"
    , "1,DEF,L,A,1000,4.34"
    , "1,GHI,L,B,500,4.34"
    , "1,GHI,M,B,2000,2.91"
    , "1,GHI,M,C,500,2.91"
    , "1,GHI,M,D,900,2.91"
    , "2,ABC,L,A,900,1.19"
    , "2,ABC,L,B,500,1.19"
    , "2,ABC,M,B,1000,2.51"
    , "2,ABC,M,C,500,2.51"
    , "2,ABC,M,D,900,2.51"
    , "2,ABC,N,A,50,0.95"
    , "2,DEF,L,A,900,4.35"
    , "2,GHI,L,B,500,4.35"
    , "2,GHI,M,B,2000,2.81"
    , "2,GHI,M,C,500,2.81"
    , "2,GHI,M,D,900,2.81"
    , "3,ABC,L,A,900,1.15"
    , "3,ABC,L,B,1500,1.15"
    , "3,ABC,M,B,1000,2.46"
    , "3,ABC,M,C,500,2.46"
    , "3,ABC,M,D,900,2.46"
    , "3,ABC,N,A,50,0.91"
    , "3,DEF,L,A,900,4.01"
    , "3,GHI,L,B,500,4.01"
    , "3,GHI,M,B,2000,2.69"
    , "3,GHI,M,C,500,2.69"
    ]

top = loc

loadLines   :: [String] -> State Loc ()
loadLines ls = do
    addDim dim { size=Fixed $ P.length ls }
    addDim dim { dimName="char", size=Ragged $ L.map P.length ls }
    addCol col { colName="value", values=T.toDyn $ concat ls }
    moveUp 2
    return ()

setDimName  :: Name -> State Loc ()
setDimName n = do
    l@(Loc { targDim=d }) <- get
    put l { targDim=d { dimName=n } }
    return ()

-- | Gets all column values grouped by shape
gather  :: T.Typeable a =>
        State Loc [[a]]
gather = do
    (Loc { targCol=c, targDim=d }) <- get
    case c of
        Just c' -> do
            let vs = T.fromDyn (values c') []
            let (Dim { size=Ragged ls }) = d
            let go [] _ = []
                go (l:ls) vs =
                    let (g,r) = splitAt l vs
                    in g:go ls r
            return $ go ls vs
        Nothing -> return []

-- | Shape creating/transforming map, creating:
-- |    - Dim #1: sibling (to targDim)
-- |    - Dim #2: child to Dim #1
-- |    - Col   : to Dim #2
smapss  :: (T.Typeable a, T.Typeable b)
        => Name
        -> ([a] -> [[b]])   -- ^ function to apply
        -> State Loc ()
smapss dn f = do
    Loc { targCol=Just Column { colName=cn' }, targDim=Dim { dimName=dn' }} <- get
    vs <- gather
    let vs' = P.map f vs
    _ <- moveUp 1
    addDim dim { dimName=dn, size=Ragged $ P.map P.length vs' }
    addDim dim { dimName=dn', size=Ragged $ P.map P.length $ concat vs' }
    addCol Column { colName=cn', values=T.toDyn $ P.concat $ P.concat vs', typ="?" }


-- | Shape eliminating map, creates:
-- |    - Col: in parent of targDim
smap    :: (T.Typeable a, T.Typeable b)
        => Name
        -> ([a] -> b)   -- ^ function to apply
        -> State Loc [b]
smap cn f = do
    vs <- gather
    let vs' = P.map f vs
    _ <- moveUp 1
    addCol Column { colName=cn, values=T.toDyn vs', typ="?" }
    return vs'


-- | Splits a sequence based on a delimeter
splitOn     :: (Eq a, T.Typeable a)
            => Name
            -> [a]          -- ^ delimeter
            -> State Loc ()
splitOn dn xs = do
    smapss dn $ L.splitOn xs


-- | Reads a column from text into data type
-- |    - Col: in parent of targDim
read    :: (Read a, T.Typeable a)
        => Name
        -> State Loc [a]
read cn = do
    vs <- smap cn P.read
    return vs


-- | Broadcast values into a flat shape
broadcastValues :: [a]          -- ^ values to broadcast
                -> Size         -- ^ shape that represent the child shape, where parent values are Top
                -> [a]          -- ^ broadcast values
broadcastValues xs (Ragged ls) = concat $ zipWith replicate ls xs
broadcastValues xs (Fixed l) = concatMap (replicate l) xs


-- | Broadcast into (replicate/fill) a dimension
{-broadcast   :: (Show a)
            => Column a     -- ^ column to be broadcast
            -> Size          -- ^ dimension
            -> Column a     -- ^ broadcast column
broadcast (Column s n xs) d = Column (Dim s $ Right d) n $ broadcastValues xs d
-}

filterSize  :: Size
            -> [Bool]
            -> Size
filterSize (Fixed l) bs = Fixed $ l - sum (P.map fromEnum bs)
filterSize (Ragged ls) bs = undefined


filterDim   :: [Bool]
            -> Dim
            -> Dim
filterDim bs d@Dim{ size=s, columns=cs, children=ds } =
    d { size=s', columns=cs', children=ds' }
    where
        s' = filterSize s bs
        cs' = M.map (filterCol bs) cs
        ds' = M.map (\d@Dim { size=s } -> filterDim (broadcastValues bs s) d) ds

filterCol   :: [Bool]
            -> Column
            -> Column
filterCol bs c@(Column { values=vs }) = c { values=T.toDyn $ go vs' bs }
    where
        vs' = T.fromDyn vs "" :: String
        go (v:vs) (b:bs) = if b then v:go vs bs else go vs bs
        go [] _ = []
        go _ [] = []

-- | Filters the current dimension
filter  :: [Bool]
        -> State Loc ()
filter bs = do
    loc <- get
    let Loc { targCol=c, targDim=d@Dim{ size=s } } = loc
        c' = liftM2 filterCol (Just bs) c
        d' = d { size=filterSize s bs }
        loc' = loc { targDim=d' }
    moveCol ""
    put loc'

-- | Multiply two sizes, with the result in the shape of the second axis
mergeSize   :: Size      -- ^ parent
            -> Size      -- ^ child
            -> Size      -- ^ total
mergeSize (Ragged ps) (Ragged cs) = Ragged $ go ps cs
    where
        go (p:ps) cs =
            let (gs, rs) = P.splitAt p cs
            in P.sum gs:go ps rs
mergeSize (Fixed p) (Ragged cs) = Ragged $ P.map (*p) cs
mergeSize (Ragged ps) (Fixed c) = Fixed $ c * P.sum ps
mergeSize (Fixed p) (Fixed c) = Fixed $ p*c

ancestors   :: State Loc [Dim]
ancestors = do
    Loc { parents=ps } <- get
    return $ P.map snd ps

mergedSize  :: State Loc Size
mergedSize = do
    let 
        go [p@Dim { size=s }] = s
        go (p@Dim { size=s }:ps) = mergeSize (go ps) s
    Loc { parents=ps } <- get
    a <- ancestors
    return $ go a


{-
across  :: [Bool]
        -> State Loc [Bool]
across (Fixed l) bs = do

    l@(Loc ) <- get

    bs [b | b <- bs, i <- [1..l]] Fixed $ l - sum (P.map fromEnum bs)

across (Ragged ls) bs = undefined

across bs = do
    loc <- get
    let Loc { targCol=c, targDim=d@Dim{ size=s } } = loc
        c' = liftM2 filterCol (Just bs) c
        d' = d { size=filterSize s bs }
        loc' = loc { targDim=d' }
    moveCol ""
    put loc'
-}

{-
load :: State Loc String
load = do
    loadLines "logEntry" ls             -- load lines of text into a dimension/column
    splitOn "field" ","                 -- split log entry fields into a new dimension

    across [True]
    filter
    _ <- smap "date_str" (!! 0)   -- extract date into it's own column
    read "date"

    s <- drawLoc
    return s
-}

{-
-- column orient data
logEntryDate = read (smap (!! 0) logEntryFieldCharValue "date") "date" :: Column Int
logEntryManagerCharValue = smap (!! 1) logEntryFieldCharValue "manager"
logEntryFundCharValue = smap (!! 2) logEntryFieldCharValue "fund"
logEntryOwnerCharValue = smap (!! 3) logEntryFieldCharValue "owner"
logEntryUnits = read (smap (!! 4) logEntryFieldCharValue "units") "units" :: Column Decimal
logEntryPrice = read (smap (!! 5) logEntryFieldCharValue "price") "price" :: Column Decimal
-}


-- | This function shows how to create an empty schema manually. Not really needed
-- | since you usually construct the schema via queries, only starting with the
-- | structures that have mirrors in the real world (eg. CSV, JSON etc.)
{-
demoSchema = do
    addDim dim { dimName="manager", size=Fixed 4 }
    addDim dim { dimName="name", size=Ragged [3,3,3,3] }
    addCol col { colName="value", values=T.toDyn $ concat ["ABC", "DEF", "GHI", "JKL" ] }
--    _ <- moveUp
    addDim dim { dimName="fund", size=Ragged [2,3,4,5] } -- 14 funds
    addCol col { colName="code", values=T.toDyn "ABCDEFGHIJKLMN" }
    addDim dim { dimName="date", size=Fixed 10 }
    addCol col { colName="date", values=T.toDyn [(1::Int)..10] }
    addCol col { colName="price", values=T.toDyn $ take 140 rand }
--    _ <- moveUp
    addDim dim { dimName="fund", size=Fixed 14 }
    addCol col { colName="correl", values=T.toDyn [(1::Int)..(14*14)] }
--    _ <- moveUp
    addDim dim { dimName="manager", size=Fixed 4 }
    addDim dim { dimName="fund", size=Ragged [2,3,4,5] } -- 14 funds
    addCol col { colName="correl", values=T.toDyn [(1::Int)..(14*14)] }
{-
    _ <- moveUp
    _ <- moveUp
    _ <- moveUp
    _ <- moveUp
-}
    get
-}

{-
main = do
    --let (l,_) = runState create loc
    --putStrLn $ drawLoc l
    let (str,st) = runState load loc
    putStrLn str
-}

drawLoc :: State Loc String
drawLoc = do
    loc@Loc { targDim=dim, targCol=tc } <- get
    let
        goLoc :: Loc -> String
        goLoc Loc { targDim=dim, siblings=ss, parents=ps } =
            L.intercalate " --> " $ P.map (dimName . snd) $ reverse ((ss,dim):ps)

        goDim :: Dim -> [String]
        goDim Dim { dimName=n, columns=cols, children=cs, size=s } =
            ("Dim: " ++ n ++ show s ++ ": " ++ goCols (M.elems cols)):goDims (M.elems cs)

        goDims :: [Dim] -> [String]
        goDims []     = []
        goDims [d]    = "|" : shift "`-- " "    " (goDim d)
        goDims (d:ds) = "|" : shift "+-- " "|   " (goDim d) ++ goDims ds

        goCols :: [Column] -> String
        goCols cs = L.intercalate ", " $ P.map colName cs

        goCol :: State Loc String
        goCol = do
            Loc { targCol=c } <- get
            case c of
                Just Column { colName=n } -> do
                    vs <- gather :: State Loc [String]
                    return $ "Col: " ++ n ++ ":" ++ show (take 6 vs)
                Nothing -> return ""

        shift first other = zipWith (++) (first : repeat other)
    c <- goCol
    return $ unlines $ ("Loc: " ++ goLoc loc):goDim dim ++ [c]


-- print (evalState create top)

{-
getDim :: Loc -> [Name] -> Loc
getDim loc [name] = intoDim loc name
getDim loc (name:ns) = getDim (intoDim loc name) ns
-- construct heirarchy
--managerSortMap = psortmap logEntryManager
--logEntrySorted = sort [logEntryManager, logEntryFund, logEntryDate] [logEntryOwner logEntryUnits, logEntryPrice]

-- | Creates a parent sort map, sorting the parent shape of the sequential sort of the children
psortmap    :: (Ord a)
            => Column a
            -> [Int]
psortmap (Column (Dim p (Right d@(Ragged _ ls))) n xs) = is'
    where
        gs = gather d xs
        ts = P.zip gs [1..]
        ts' = P.sortWith fst ts
        is' = P.map snd ts'

-- | Creates a sort map, being a list of integer indexes that must be applied to
-- | sort the data.
sortmap :: (Ord a)
        => Column a
        -> [Int]
sortmap (Column s n xs) = is'
    where
        ts = P.zip xs [1..]
        ts' = P.sortWith fst ts
        is' = P.map snd ts'

-- | Cannonicalize space into simplest form
cannon  :: Dim
        -> Dim
cannon (Dim Root (Left Root)) = Root
cannon (Dim p (Left Root)) = cannon p
cannon (Dim p (Left (Dim Root s''))) = Dim p s''
cannon (Dim p (Left s')) = Dim p $ Left $ cannon s'
cannon (Dim p s) = Dim (cannon p) s
cannon s = s

slice   :: Int
        -> Int
        -> [Bool]
slice = go
    where
        go b e
            | b > 0 = False:go (b-1) (e-1)
            | e > 0 = True:go (b-1) (e-1)
            | otherwise = []

-- | Limits the depth of shape a at given ancestor
-- |    total: entire shape
-- |    ancestor: higher shape
-- |    child: sub-shape, parent represented by Root
-- @
--
-- @
-- TRUTH:   t = a `join` c
--          c = t `splitS` a
--          curry $ join a (limitS c a) == c
limitS  :: Dim        -- ^ child: space to limit
        -> Dim        -- ^ ancestor: root ancestor to retain
        -> Dim        -- ^ dimension with no ancestors beyond `ancestor`
limitS c a | c == a = Dim Root $ Left c
limitS (Dim p s) a = Dim (p `limitS` a) s
limitS c _ = c
(-|) = limitS

limitC  :: Column a     -- ^ child: space to limit
        -> Dim        -- ^ ancestor: root ancestor to retain
        -> Column a     -- ^ result with no ancestors beyond `ancestor`
limitC (Column s n xs) a = Column (s `limitS` a) n xs
(=|) = limitC

-- | Dim together an ancestor and child dimension, where the child refers to the ancestor as Top
join    :: Dim        -- ^ ancestor (the new Dim's Root)
        -> Dim        -- ^ child
        -> Dim
join t Root = t
join t (Dim p (Left s)) = Dim (join t p) $ Left (join t s)
join _ c = c
(>>) = join

-- | Create heirarchy that describes the difference between total and child
-- |    total = parent + child
-- |
-- TRUTH: p = inflate t c
--        t = deflate p c
inflate :: Size      -- ^ total
        -> Size      -- ^ child
        -> Size      -- ^ parent
inflate (Ragged tn ms) (Ragged cn ds) = Ragged cn $ go ms ds 0 0
    where
        go (m:ms) (d:ds) a i
            | m == a + d    = (i+1):go ms ds 0 0        -- new group
            | m > a + d     = go (m:ms) ds (a+d) (i+1)  -- no new group yet
            | otherwise     = undefined                 -- group overrun?
inflate (Fixed tn ml) (Fixed cn dl)
    | ml `mod` dl == 0      = Fixed cn $ ml `div` dl
    | otherwise             = undefined                 -- doesn't fit!
inflate (Fixed tn ml) (Ragged cn ds) = Ragged cn $ go ml ds 0 0
    where
        go ml (d:ds) a i
            | ml == a + d   = (i+1):go ml ds 0 0        -- new group
            | ml > a + d    = go ml ds (a+d) (i+1)      -- no new group yet
            | otherwise     = undefined                 -- group overrun?

-- | Merge two dimensions together
merge   :: Size      -- ^ parent
        -> Size      -- ^ child
        -> Size      -- ^ total
merge (Ragged pn hs) (Ragged cn ds) = Ragged cn $ go hs ds
    where
        go (h:hs) ds =
            let (gs, rs) = P.splitAt h ds
            in P.sum gs:go hs rs
merge (Fixed pn h) (Ragged cn ds) = Fixed cn $ h * P.sum ds
merge (Ragged pn hs) (Fixed cn d) = Ragged cn $ P.map (*d) hs
merge (Fixed pn hs) (Fixed cn ds) = Fixed cn $ hs * ds

-- | Broadcast values into a flat shape
broadcastValues :: [a]          -- ^ values to broadcast
                -> Size          -- ^ shape that represent the child shape, where parent values are Top
                -> [a]          -- ^ broadcast values
broadcastValues xs (Ragged _ ls) = concat $ zipWith replicate ls xs
broadcastValues xs (Fixed _ l) = concatMap (replicate l) xs

-- | Broadcast into (replicate/fill) a dimension
broadcast   :: (Show a)
            => Column a     -- ^ column to be broadcast
            -> Size          -- ^ dimension
            -> Column a     -- ^ broadcast column
broadcast (Column s n xs) d = Column (Dim s $ Right d) n $ broadcastValues xs d

-- | Group children into a new parent dimension
group   :: Column a
        -> Column a
group = undefined

-- | Flat map
map :: (a -> b)     -- ^ function to apply
    -> Column a     -- ^ source column
    -> Name
    -> Column b     -- ^ mapped column
map f (Column s _ xs) n = Column s n $ P.map f xs

-- | Shape eliminating map
smap    :: ([a] -> b)   -- ^ function to apply
        -> Column a     -- ^ source column
        -> Name         -- ^ new column name
        -> Column b     -- ^ mapped column in parent's space
smap f c@(Column (Dim p (Right d)) _ xs) n = Column p n xs'
    where
        xs' = P.map f $ gather d xs

-- | Shape introducing map
maps    :: (a -> [b])   -- ^ function to apply
        -> Column a     -- ^ source column
        -> Name         -- ^ new dimension name
        -> Name         -- ^ new column name
        -> Column b     -- ^ mapped column in new child dimension
maps f (Column s _ xs) nd nc = Column s' nc xs'
    where
        ys = P.map f xs
        d' = Ragged nd $ P.map P.length ys
        s' = Dim s $ Right d'
        xs' = concat ys

-- | Gets all column values grouped by dimension
gather  :: Size
        -> [a]
        -> [[a]]
gather (Ragged _ ls) xs = go ls xs
    where
        go [] _ = []
        go (l:ls) xs =
            let (g,r) = splitAt l xs
            in g:go ls r
gather (Fixed _ l) xs = go xs
    where
        go xs =
            let (g,r) = splitAt l xs
            in g:go r

values1 (Column (Dim p (Right d)) n xs) = gather d xs
values2 (Column (Dim (Dim pp (Right d1)) (Right d0)) n xs) = gather d1 (gather d0 xs)

dimOf       :: [[a]]
            -> Name
            -> Size
dimOf xss n = Ragged n $ P.map P.length xss

-- | Shape creating/transforming map: creates these dimensions:
-- |    - sibling (to source)
-- |    - nephew (to source)
smapss  :: ([a] -> [[b]])   -- ^ function to apply
        -> Column a         -- ^ source column
        -> Name             -- ^ new sibling dimension name
        -> Column b         -- ^ mapped column in new nephew column
smapss f (Column (Dim p (Right d@(Ragged n ls))) nc xs) nd = Column ns nc x
    where
        gs = gather d xs
        xss' = P.map f gs
        xs' = concat xss'
        ss = Dim p $ Right $ dimOf xss' nd    -- sibling
        ns = Dim ss $ Right $ dimOf xs' n     -- nephew
        x = concat xs'

-- | Shape transforming map: map from shape to sibling
smaps  :: ([a] -> [b])      -- ^ function to apply
        -> Column a         -- ^ source column
        -> Name             -- ^ new sibling dimension name
        -> Name             -- ^ new column name
        -> Column b         -- ^ mapped column in new sibling dimension
smaps f (Column (Dim p (Right d@(Ragged _ ls))) _ xs) nd nc = Column s' nc xs'
    where
        gs = gather d xs
        d' = dimOf gs nd
        s' = Dim p $ Right d'
        xs' = concatMap f gs


-- | Flat scan
scan    :: (b -> a -> b)    -- ^ aggregation function
        -> b                -- ^ starting seed for each group
        -> Column a         -- ^ source
        -> Column b         -- ^ result
scan f seed (Column d n xs) = Column d n (P.scanl f seed xs)

-- | Zip
zip :: Column a
    -> Column b
    -> Column (a,b)
zip (Column ad an as) (Column bd bn bs)
    | ad == bd = Column ad (an ++ "=" ++ bn) $ P.zip as bs
    -- | ancestor
    -- | distant relative

-- | Fold into parent dimension
fold    :: (a -> b -> b)    -- ^ aggregation function
        -> b                -- ^ start seed state
        -> Column a         -- ^ source column
        -> Name
        -> Column b
fold f seed = smap $ P.foldr f seed

-- | Append Data
append  :: Column a
        -> a
        -> IO ()
append = undefined

{-
-- | Gets the length of a dimension
length :: Shape
        -> Int
length Root = 1
length Dim p (Right (Fixed l)) = (lengthS p) * l
length Dim p (Right (Ragged ls)) = (lengthS p) * (sum ls)
length Dim p s = (lengthS)
-}

-- | Creates a sub-dimension that only includes certain values
boolDim :: Column Bool
        -> Size
boolDim (Column s n bs) = Ragged n $ P.map fromEnum bs

-- | Splits a column into a sibling space
splitOn     :: (Eq a)
            => [a]          -- ^ sequence to split on
            -> Column a     -- ^ source column
            -> Name         -- ^ name of sibling dimension
            -> Column a
splitOn = smapss . L.splitOn

-- helpers:
read    :: (Read a)
        => Column Char
        -> Name
        -> Column a
read = smap P.read

last    :: Column a
        -> Name
        -> Column a
last c n = smaps f c n n
    where
        f [] = []
        f [x] = [x]
        f (_:xs) = f xs

-- | Loads some lines into a new column:
-- |    Root -> Line -> Char (Column Char)
loadLines   :: Name
            -> [String]
            -> Column Char
loadLines n ls = Column charShape "value" cs
    where
        lineShape = Dim Root $ Right $ Fixed n $ P.length ls
        charShape = Dim lineShape $ Right $ dimOf ls "char"
        cs = concat ls
-}
