


loadFile :: FilePath -> IO [(Int, Int)]
loadFile path
 = do	file		<- readFile path
	let nVals	=  map parseLine $ filter (\l -> head l /= '#') $ lines file
	return nVals


parseLine :: String -> (Int, Int)
parseLine str
 = let 	[n, x]	= map read $ words str
   in	(n, x)

	
binVals :: (Ord a, Enum a, Num a) => a -> a -> a -> [a] -> [(a, a, Int)]	
binVals start end gap vals
 = let	edges	= [start, start+gap .. end]
   	bins	= [(start, start + gap, 0) | start <- edges]
   in	foldr accBin bins vals
		
accBin :: (Ord a, Num a) => a -> [(a, a, Int)] -> [(a, a, Int)]
accBin val []	= error "no bin"
accBin val (bin@(start, end, count):rest)
	| val >= start
	, val <  end
	= (start, end, count + 1) : rest
	
	| otherwise
	= bin : accBin val rest
	

	