
module Profile (
	profileAccumulatePreReduce,
	profileAccumulatePostReduce,

	tallyThreadTags,

	plotRunIntervals,
	plotThreads,
	plotThreadsSolid,

	plotAsImpulses,
	plotAsLines
)

where

import	Array

import	Type
import	Heap
import	Schedule
import	Util

import	Debug.Trace

--------------------------------------------------------------------------------------------------------------- accumulateProfData
profileAccumulatePostReduce ::	State	-> State
profileAccumulatePostReduce	state	=
 let
	tagsActive			= zip (sThreadsActive  state) (map ThreadActive (sThreadsActiveRules state))
	tagsWaiting			= zip (sThreadsWaiting state) (repeat ThreadWaiting)
	tagsBlocked			= zip (sThreadsBlocked state) (repeat ThreadBlocked)
	tags				= tagsActive ++ tagsWaiting ++ tagsBlocked
 in
	state {
		sProfThreadTags		= tags				 : sProfThreadTags      state,

		sProfUnblockQueueLength	= length (sUnblockQueue state)   : sProfUnblockQueueLength state,
		sProfSpawnQueueLength	= length (sSpawnQueue   state)   : sProfSpawnQueueLength   state

	}


profileAccumulatePreReduce ::	State	-> State
profileAccumulatePreReduce	state	=
 let
	heapCellsUsed		= hCells (sHeap state) - hFreeCells (sHeap state)

	blockedSteps		= 
		if length (sThreadsActive state) == 0 
			then (sProfBlockedSteps state) + 1 
			else (sProfBlockedSteps state)

	blockedReductions	= (sProfBlockedReductions state) + length (sThreadsBlocked state)


 in
	state { 
		sProfBlockedSteps	= blockedSteps,
		sProfBlockedReductions	= blockedReductions,
		sProfHeapCellsUsed	= heapCellsUsed			 : sProfHeapCellsUsed   state, 

		sProfThreadsActive	= length (sThreadsActive  state) : sProfThreadsActive   state,
		sProfThreadsWaiting	= length (sThreadsWaiting state) : sProfThreadsWaiting  state,
		sProfThreadsBlocked	= length (sThreadsBlocked state) : sProfThreadsBlocked  state
 	}



-------------------------------------------------------------------------------------------------------------- tallyThreadTags
tallyThreadTags ::	[ThreadTag] -> 	[(ThreadTag, Int)]
tallyThreadTags		threadTags	
	=  chain incrementTally [] threadTags

incrementTally ::	[(ThreadTag, Int)] -> 	ThreadTag	-> [(ThreadTag, Int)]
incrementTally		tagTallys		tag		=
 let
	tags		= map fst tagTallys
	thisTagIx	= must $ indexOf tags tag
	thisTagTally	= tagTallys !! thisTagIx
	
	cutTagTallys	= dropElemAtIx thisTagIx tagTallys
 in
	if not $ elem tag tags
		then	(tag, 1) 			: tagTallys
		else	(tag, snd thisTagTally + 1) 	: cutTagTallys


---------------------------------------------------------------------------------------------------------------------- plotThreads
plotThreads ::		String -> 	(Int, Int) ->		State ->	IO ()
plotThreads		fileNamePrefix	(startCycle, endCycle)	state		=
 let
	plotCommand	=  "set xrange [" ++ show startCycle ++ ":" ++ show endCycle ++ "]\n"
			++ "set yrange [0:]\n"
			++ "set nokey\n"
			++ "plot "
			++ "'" ++ fileNamePrefix ++ ".data.blocked'  with lines lt 1,"
			++ "'" ++ fileNamePrefix ++ ".data.waiting'  with lines lt 3,"
			++ "'" ++ fileNamePrefix ++ ".data.active'   with lines lt 2\n"
 in
  do
	writeFile 	(fileNamePrefix ++ ".data.blocked") 
			(concat $ map (\x -> show x ++ "\n") $ reverse  $ sProfThreadsBlocked  state)

	writeFile 	(fileNamePrefix ++ ".data.active") 
			(concat $ map (\x -> show x ++ "\n") $ reverse  $ sProfThreadsActive   state)

	writeFile 	(fileNamePrefix ++ ".data.waiting") 
			(concat $ map (\x -> show x ++ "\n") $ reverse  $ sProfThreadsWaiting  state)

	writeFile	(fileNamePrefix ++ ".gnuplot") plotCommand


plotThreadsSolid ::	String -> 	(Int, Int) ->		State ->	IO ()
plotThreadsSolid	fileNamePrefix	(startCycle, endCycle)	state		=
 let
	plotCommand	=  "set xrange [" ++ show startCycle ++ ":" ++ show endCycle ++ "]\n"
			++ "set yrange [0:]\n"
			++ "set nokey\n"
			++ "plot "
			++ "'" ++ fileNamePrefix ++ ".data.blocked' with impulses lt 1,"
			++ "'" ++ fileNamePrefix ++ ".data.waiting' with impulses lt 3,"
			++ "'" ++ fileNamePrefix ++ ".data.active'  with impulses lt 2\n"

	threadsActive	= reverse $ sProfThreadsActive  state
	threadsWaiting	= reverse $ sProfThreadsWaiting state
	threadsBlocked	= reverse $ sProfThreadsBlocked state

	layer0		= threadsActive
	layer1		= zipWith (+) layer0 threadsWaiting
	layer2		= zipWith (+) layer1 threadsBlocked
 in
  do
	writeFile 	(fileNamePrefix ++ ".data.active") 
			(concat $ map (\x -> show x ++ "\n") $ layer0)

	writeFile 	(fileNamePrefix ++ ".data.waiting") 
			(concat $ map (\x -> show x ++ "\n") $ layer1)

	writeFile 	(fileNamePrefix ++ ".data.blocked") 
			(concat $ map (\x -> show x ++ "\n") $ layer2)

	writeFile	(fileNamePrefix ++ ".gnuplot") plotCommand


------------------------------------------------------------------------------------------------------------------- plotAsImpulses
plotAsLines ::		String ->	[(Int, Int)]		-> IO ()
plotAsLines 		fileName	xx			=
 let
	minx		= minimum (map fst xx)
	maxx		= maximum (map fst xx)
	maxy		= maximum (map snd xx)

	plotCommand	=  "set xrange [" ++ show minx 	++ ":" ++ show maxx 		++ "]\n"
			++ "set yrange [" ++ "0" 	++ ":" ++ show (maxy+ 1)	++ "]\n"
			++ "plot "
			++ "'" ++ fileName ++ "' with lines\n"
 in
  do
	writeFile	fileName
			(concat $ map (\(ix, elem) -> show ix ++ " " ++ show elem ++ "\n") xx)

	writeFile	(fileName ++ ".gnuplot") plotCommand


plotAsImpulses :: 	String ->	[(Int, Int)] 		-> IO ()
plotAsImpulses		fileName	xx 			=
 let
	minx		= minimum (map fst xx)
	maxx		= maximum (map fst xx)
	maxy		= maximum (map snd xx)

	plotCommand	=  "set xrange [" ++ show minx 	++ ":" ++ show maxx 		++ "]\n"
			++ "set yrange [" ++ "0" 	++ ":" ++ show (maxy+ 1)	++ "]\n"
			++ "plot "
			++ "'" ++ fileName ++ "' with impulses\n"
 in
  do
	writeFile	fileName
			(concat $ map (\(ix, elem) -> show ix ++ " " ++ show elem ++ "\n") xx)

	writeFile	(fileName ++ ".gnuplot") plotCommand


----------------------------------------------------------------------------------------------------------------- plotRunIntervals
plotRunIntervals :: 	String ->	(Int, Int) ->		State	-> IO ()
plotRunIntervals	fileNamePrefix	(startCycle, endCycle)	state	=
 let
	maxActiveThreadNum	= maximum $ map length (sProfThreadTags state)

	setCommand namePrefix lineType
			=  "'" ++ namePrefix ++ ".0' with lines " ++ lineType
			++ (concat 
				$ map (\tn -> ", '" ++ namePrefix ++ "." ++ show tn ++ "' with lines " ++ lineType) 
					[0 .. maxActiveThreadNum])

	plotCommand	=  "set xrange [" ++ show startCycle ++ ":" ++ show endCycle ++ "]\n"
			++ "set yrange [-1:" ++ show (maxActiveThreadNum + 1) ++ "]\n"
			++ "set nokey\n"
			++ "set ytics 1\n"
			++ "set linestyle 1 lt 1 linewidth 4\n"
			++ "set linestyle 2 lt 2 linewidth 4\n"
			++ "set linestyle 3 lt 3 linewidth 4\n"
			++ "plot " ++ setCommand (fileNamePrefix ++ ".active")  "ls 2"
			++ ", "	   ++ setCommand (fileNamePrefix ++ ".waiting") "ls 3"
			++ ", "    ++ setCommand (fileNamePrefix ++ ".blocked") "ls 1" ++ "\n"
 in
  do
	mapM_	(plotThreadIntervals (fileNamePrefix ++ ".active") 
		(getThreadIntervals getTRSActive) state) 
		[0 .. maxActiveThreadNum]

	mapM_	(plotThreadIntervals (fileNamePrefix ++ ".waiting") 
		(getThreadIntervals getTRSWaiting) state) 
		[0 .. maxActiveThreadNum]

	mapM_	(plotThreadIntervals (fileNamePrefix ++ ".blocked") 
		(getThreadIntervals getTRSBlocked) state) 
		[0 .. maxActiveThreadNum]

	writeFile (fileNamePrefix ++ ".gnuplot") plotCommand



plotThreadIntervals ::	  String ->		([ThreadTag] -> [(Int, Int)]) ->State -> 	ThreadNum	-> IO ()
plotThreadIntervals	  fileNamePrefix	getTRS				state		threadNum	=
 let
	intervals	= getTRS
			$ getTagsForThread threadNum 
			$ reverse $ sProfThreadTags state

	plotData	= concat $ map (showInterval threadNum) intervals
 in
	writeFile (fileNamePrefix ++ "." ++ (show threadNum)) plotData


showInterval :: 	Int ->	(Int, Int)	-> String
showInterval		level	(start, end)	
	=  (show start) ++ " " ++ (show level) ++ "\n" 
	++ (show end)   ++ " " ++ (show level) ++ "\n\n"


getTagsForThread ::	Int -> 		[[(Int, ThreadTag)]]	-> [ThreadTag]
getTagsForThread	threadNum	threadNumTags		= 
	map (activityToTag threadNum) threadNumTags


activityToTag ::	Int ->		[(Int, ThreadTag)]	-> ThreadTag
activityToTag		threadNum	threadNumTags		=
	case lookup threadNum threadNumTags of 
		Nothing		-> ThreadInactive
		Just tag	-> tag


getThreadIntervals :: 	([ThreadTag] -> [Int]) -> 	[ThreadTag] 	-> [(Int, Int)]
getThreadIntervals	getTRS				threadTags	=
 let
	rs	= getTRS threadTags
 in
	if rs == []
		then []
		else map toTuple2 $ glob 2 rs



getTRSWaiting ::	[ThreadTag]	-> [Int]
getTRSWaiting		threadTags	= getTRSWaiting' 0 ThreadInactive threadTags

getTRSWaiting' ::	Int ->		ThreadTag ->	 [ThreadTag]			-> [Int]
getTRSWaiting'		stepCount	ThreadWaiting	 []				= [stepCount]
getTRSWaiting'		stepCount	_		 (ThreadInactive      : [])	= []
getTRSWaiting'		stepCount	_ 		 (ThreadActive "stop" : [])	= []

getTRSWaiting' 		stepCount 	ThreadWaiting 	 (ThreadWaiting : xs)		= 		
 getTRSWaiting'		(stepCount + 1) ThreadWaiting 	 xs

getTRSWaiting' 		stepCount 	_                (ThreadWaiting : xs)		= stepCount : 	
 getTRSWaiting'		(stepCount + 1) ThreadWaiting 	 xs

getTRSWaiting' 		stepCount 	ThreadWaiting 	 (x		: xs)		= stepCount : 	
 getTRSWaiting'		(stepCount + 1) x		 xs

getTRSWaiting' 		stepCount 	_                (x              : xs)		= 		
 getTRSWaiting'		(stepCount + 1) x		 xs




getTRSActive ::		[ThreadTag]	-> [Int]
getTRSActive		threadTags	= getTRSActive' 0 ThreadInactive threadTags

getTRSActive' ::	Int ->		ThreadTag ->	 [ThreadTag]			-> [Int]
getTRSActive'		stepCount	_		 (ThreadInactive      : [])	= []
getTRSActive'		stepCount	_ 		 (ThreadActive "stop" : [])	= [stepCount]

getTRSActive' 		stepCount 	(ThreadActive _) (ThreadActive n : xs)		= 		
 getTRSActive'		(stepCount + 1) (ThreadActive n) xs

getTRSActive' 		stepCount 	_                (ThreadActive n : xs)		= stepCount : 	
 getTRSActive' 		(stepCount + 1) (ThreadActive n) xs

getTRSActive' 		stepCount 	(ThreadActive n) (x		: xs)		= stepCount : 	
 getTRSActive' 		(stepCount + 1) x		 xs

getTRSActive' 		stepCount 	_                (x              : xs)		= 		
 getTRSActive' 		(stepCount + 1) x		 xs


getTRSBlockedd		threadTags	= getTRSBlocked $ trace (show threadTags) threadTags

getTRSBlocked ::	[ThreadTag]	-> [Int]
getTRSBlocked		threadTags	= getTRSBlocked' 0 ThreadInactive threadTags

getTRSBlocked' ::	Int ->		ThreadTag ->	  [ThreadTag]			-> [Int]
getTRSBlocked'		stepCount	ThreadBlocked	  []				= [stepCount]
getTRSBlocked'		stepCount	_		  (ThreadInactive      : [])	= []
getTRSBlocked'		stepCount	_ 		  (ThreadActive "stop" : [])	= []

getTRSBlocked' 		stepCount 	ThreadBlocked     (ThreadBlocked       : xs)	= 		
 getTRSBlocked'		(stepCount + 1) ThreadBlocked     xs

getTRSBlocked' 		stepCount 	_                 (ThreadBlocked       : xs)	= stepCount : 	
 getTRSBlocked' 	(stepCount + 1) ThreadBlocked     xs

getTRSBlocked' 		stepCount 	ThreadBlocked 	  (x		: xs)		= stepCount : 	
 getTRSBlocked' 	(stepCount + 1) x		  xs

getTRSBlocked' 		stepCount 	_                 (x            : xs)		= 		
 getTRSBlocked' 	(stepCount + 1) x		  xs








