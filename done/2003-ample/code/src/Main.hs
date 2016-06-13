module Main where


---------------------------------------------------------------------------------------------------------------------------- Main.hs
import	IO
import	Array
import	System

import	Util
import	Type
import	Lexer
import	Parser
import	ProgUtil
import	ConvertExp
import	Trimmers
import	Pretty
import  Profile
import	Heap
import	Init
import	Heap
import	Garbage
import	Schedule
import	Step
import	ReduceThread

import	qualified ReduceThread_single
import	qualified ReduceThread_fullspec
import	qualified ReduceThread_parseq
	
fileAmpleBanner		= "doc/ampleBanner.txt"
fileAmpleHelp		= "doc/ampleHelp.txt"
filePrelude		= "include/prelude.ae"
dirProf			= "prof/"

----------------------------------------------------------------------------------------------------------------------------- main
main	= 


 do
	banner	<- readFile fileAmpleBanner
	putStr banner

	let prog0	= emptyProg (msMaxProgCells mainStateBlank)
	mainLoop mainStateBlank (startState prog0 0 mainStateBlank)



------------------------------------------------------------------------------------------------------------------------ MainState
data MainState 
	= MainState {
		msTraceLevel			:: Int,
		msProf				:: Bool,
	
		msModeName			:: String,
		msReduceThread			:: State -> Int -> (State, ([ReduceMod], String)),

		msUnblockDelay			:: Int,
		msUnblockSchedFunc		:: forall a. Int -> [a] -> [(Int, a)] -> [(Int, a)],

		msSpawnDelay			:: Int,
		msSpawnSchedFunc		:: forall a. Int -> [a] -> [(Int, a)] -> [(Int, a)],

		msMaxHeapCells			:: Int,
		msMaxProgCells			:: Int,

		msMaxThreads			:: Int,
		msMaxThreadsActive		:: Int,

		msAutoRun			:: Bool,
		msAutoTrimmers			:: Bool,
		msStepsToGo			:: Int,

		msBreakStep			:: Int

	}	

mainStateBlank	
	= MainState {
		msTraceLevel			= 0,
		msProf				= True,

		msModeName			= "single",
		msReduceThread			= ReduceThread_fullspec.reduceThread,
		
		msUnblockDelay			= 0,
		msUnblockSchedFunc		= scheduleImmediate,

		msSpawnDelay			= 0,
		msSpawnSchedFunc		= scheduleImmediate,

		msMaxHeapCells			= 1000,
		msMaxProgCells			= 2000,

		msMaxThreads			= 100,
		msMaxThreadsActive		= 100,

		msAutoRun			= True,
		msAutoTrimmers			= True,
		msStepsToGo			= 0,

		msBreakStep			= 0
	}


---------------------------------------------------------------------------------------------------------------------- slurpSource
slurpSource ::	String	-> IO String
slurpSource	filename	= do
	source	<- readFile filename
	
	return source



------------------------------------------------------------------------------------------------------------------------- mainLoop
mainLoop mainState state =
 do
  putStr	"> "
  hFlush	stdout

  input		<- getLine
  let	arg	= chomp input

  case arg !! 0 of 
	""		-> 
	 do
		if not $ msAutoRun mainState 
 			then do
				nState	<- doStep mainState state
				mainLoop	  mainState nState
			else do
				mainLoop 	mainState state

	":help"		-> 
	 do
		help	<- readFile fileAmpleHelp
		putStr help
		mainLoop mainState state


-- run control

	":run"		->
	 if length arg == 1 
 		then do
			let nMainState	= mainState { msAutoRun = True,  msStepsToGo = 0 }
	 		nState		<- run	nMainState	state { sRun = True }
			mainLoop nMainState nState

		else do
			let nMainState	= mainState { msAutoRun = False, msStepsToGo = read (arg !! 1) }
			nState		<- run	nMainState	state { sRun = True }
			mainLoop nMainState nState


	":runToStep"	->
	 do
		nState	<- run mainState { msBreakStep = read (arg !! 1) } state { sRun = True }
		mainLoop mainState nState


	":setAutoRun"	-> 
	 do
 		mainLoop mainState { msAutoRun = read (arg !! 1) } state

	":step"		-> 
	 do
		nState	<- doStep	mainState state
		mainLoop mainState nState


-- machine setup

	":setMaxThreads" ->
	 do
		mainLoop mainState { msMaxThreads = read (arg !! 1) } state


	":setMaxHeapCells" ->
	 do
		mainLoop mainState { msMaxHeapCells = read (arg !! 1) } state


	":setMaxThreadsActive" ->
	 do
		mainLoop mainState { msMaxThreadsActive = read (arg !! 1) } state


	":setMode"      ->
		if      (arg !! 1) == "single"   
			then mainLoop 
				mainState { 
					msModeName	= "single",   
					msReduceThread	= ReduceThread_single.reduceThread }
				state

		else if (arg !! 1) == "fullspec" 
			then mainLoop 
				mainState { 
					msModeName 	= "fullspec", 
					msReduceThread 	= ReduceThread_fullspec.reduceThread }
				state

		else if (arg !! 1) == "parseq"   
			then mainLoop 
				mainState { 
					msModeName 	= "parseq",
					msReduceThread	= ReduceThread_parseq.reduceThread }
				state

		else do
			putStr ("bad mode name '" ++ (arg !! 1) ++ "'\n\n")
			mainLoop mainState state


	":setUnblockDelay" ->
		mainLoop mainState { msUnblockDelay    = read (arg !! 1) } state


	":setUnblockSchedFunc" ->
		case (arg !! 1) of
			"immediate"	-> mainLoop mainState { msUnblockSchedFunc = scheduleImmediate  } state
			"sequential"	-> mainLoop mainState { msUnblockSchedFunc = scheduleSequential } state
			_		-> 
			 do
				putStr ("bad schedFunc name '" ++ arg !! 1 ++ "'\n")
				mainLoop mainState state


	":setSpawnDelay" ->
		mainLoop mainState { msSpawnDelay      = read (arg !! 1) } state


	":setSpawnSchedFunc" ->
		case (arg !! 1) of
			"immediate"	-> mainLoop mainState { msSpawnSchedFunc = scheduleImmediate  } state
			"sequential"	-> mainLoop mainState { msSpawnSchedFunc = scheduleSequential } state
			_		->
			 do
				putStr ("bad schedFunc name '" ++ arg !! 1 ++ "'\n")
				mainLoop mainState state

-- machine status

	":state"	-> 
  	 do
		printState state
		mainLoop mainState state

	":heap"		-> 
	 do
		putStr	(heapShow (sHeap state) ++ "\n")
		mainLoop mainState state


	":prog"		-> 
	 do
		putStr	(heapShow (sProg state) ++ "\n")
		mainLoop mainState state


	":progc"	-> 
	 do
		let	eix		= read (arg !! 1)
  		let	cell		= heapGet (sProg state) eix
		putStr	(show cell ++ "\n\n") 
		mainLoop mainState state


	":pretty"	-> 
	 do
		let	eix		= read (arg !! 1)
		putStr	(prettyExp (sProg state) eix ++ "\n\n")
		mainLoop mainState state	


-- profiling

	":trace"	-> 
	 do
		let	level		= read (arg !! 1)
		let	nMainState	= mainState { msTraceLevel = level }
		mainLoop nMainState state


	":setProf"	-> 
	 do
		mainLoop mainState { msProf 	= read (arg !! 1) } state





	":tally"	->
	 do
		putStr  $ concat
			$ map (\x -> show x ++ "\n")
			$ tallyThreadTags 
			$ map snd 
			$ concat 
			$ sProfThreadTags state

		putStr	"\n"

		mainLoop mainState state

	":plotThreadsLine" ->
	 let
		interval	= if (length arg == 1) 
					then (0, sSteps state) 
					else (read $ arg !! 1, read $ arg !! 2)

		fileNamePrefix	= dirProf ++ "thread"
	 in
	  do
		plotThreads fileNamePrefix interval state
		_	<- system ("gnuplot -persist -geometry 500x200 " ++ fileNamePrefix ++ ".gnuplot")
		mainLoop mainState state


	":plotThreads" ->
	 let
		interval	= if (length arg == 1) 
					then (0, sSteps state) 
					else (read $ arg !! 1, read $ arg !! 2)

		fileNamePrefix	= dirProf ++ "threadSolid"
	 in
	  do
		plotThreadsSolid fileNamePrefix interval state
		_	<- system ("gnuplot -persist -geometry 500x200 " ++ fileNamePrefix ++ ".gnuplot")
		mainLoop mainState state



	":plotActivity" -> 
	 let
		interval	= if (length arg == 1) 
					then (0, sSteps state) 
					else (read $ arg !! 1, read $ arg !! 2)

		fileNamePrefix	= dirProf ++ "thread.runIntervals"
	 in
 	  do
		plotRunIntervals fileNamePrefix interval state
		_	<- system ("gnuplot -persist -geometry 500x200 " ++ fileNamePrefix ++ ".gnuplot")
		mainLoop mainState state


	":plotUnblockQueueLength" ->
	 let
		interval	= if (length arg == 1)
					then (0, sSteps state)
					else (read $ arg !! 1, read $ arg !! 2)

		fileNamePrefix	= dirProf ++ "unblockQueue.length"
	 in
	  do	
		plotAsImpulses 	fileNamePrefix 
				(takeRange	interval
						(zip [0 ..] (reverse $ sProfUnblockQueueLength state)))

		_	<- system ("gnuplot -persist -geometry 500x100 " ++ fileNamePrefix ++ ".gnuplot")
		mainLoop mainState state


	":plotSpawnQueueLength" ->
	 let
		interval	= if (length arg == 1)
					then (0, sSteps state)
					else (read $ arg !! 1, read $ arg !! 2)

		fileNamePrefix	= dirProf ++ "spawnQueue.length"
	 in
	  do
		plotAsImpulses	fileNamePrefix
				(takeRange	interval
						(zip [0 ..] (reverse $ sProfSpawnQueueLength state)))

		_	<- system ("gnuplot -persist -geometry 500x100 " ++ fileNamePrefix ++ ".gnuplot")
		mainLoop mainState state


	":plotHeapCellsUsed" ->
	 let
		fileNamePrefix	= dirProf ++ "heap.cellsUsed"
		
		interval	= if (length arg == 1)
					then (0, sSteps state)
					else (read $ arg !! 1, read $ arg !! 2)
	 in
	  do	
		plotAsLines 	fileNamePrefix 
				(takeRange	interval
						(zip [0 ..] (reverse $ sProfHeapCellsUsed state)))

		_	<- system ("gnuplot -persist -geometry 500x100 " ++ fileNamePrefix ++ ".gnuplot")
		mainLoop mainState state


-- compilation

	":slurpEixs"	->
	 do
 		let	eix		= read (arg !! 1)
		let	cell		= heapGet (sProg state) eix
		let	exp		= pcExp cell
		let	eixs		= slurpEixs exp
		putStr	("eixs  = " ++ show eixs ++ "\n\n")
		mainLoop mainState state

	":setAutoTrimmers"	->
	 do
		mainLoop mainState { msAutoTrimmers = read (arg !! 1) } state


	":depthTag"	->
	 do
		let	eix		= read (arg !! 1)
		let	nState		= state { sProg = depthTag (sProg state) eix }
		mainLoop mainState nState


	":buildTrimmer" ->
	 do
 		let	eix		= read (arg !! 1)
		let	nState		= state { sProg = buildTrimmer (sProg state) eix }
		mainLoop mainState nState


	":buildTrimmers" ->
	 do
 		let	eix		= read (arg !! 1)
		let	nState		= state { sProg = buildTrimmers (sProg state) eix }
		mainLoop mainState nState
					

-- garbage collection

	":deps"		-> 
	 do
		let	deps		= stateDeps state
		putStr	("deps  = " ++ show deps ++ "\n\n")
		mainLoop mainState state


	":gc"		->
	 do
		let	oldFreeCells	= hFreeCells (sHeap state)
		let	nState		= garbageCollect state
		let	newFreeCells	= hFreeCells (sHeap nState)

		putStr	("cells reclaimed = " ++ show (newFreeCells - oldFreeCells) ++ "\n\n")
		mainLoop mainState nState


	":mark"		->
	 do
		let	deps		= read (concat (tail arg))
		let	nHeap		= markDeps	(sHeap state) deps
		let	nState		= state { sHeap = nHeap }
		mainLoop mainState nState


	":sweep"	->
	 do
		let	oldFreeCells	= hFreeCells (sHeap state)
		let	nHeap		= sweepCells	(sHeap state)
		let	nState		= state { sHeap = nHeap }
		let	newFreeCells	= hFreeCells (sHeap nState)

		putStr ("cells reclaimed = " ++ show (newFreeCells - oldFreeCells) ++ "\n\n")
		mainLoop mainState nState
		

	":clear"	-> 
	 do
		let	nHeap		= clearAllMarks	(sHeap state)
		let	nState		= state { sHeap = nHeap }
		mainLoop mainState nState




	(':':ss)	-> 
	 do
		putStr ("error: unknown ample directive '" ++ ss ++ "'\n\n")
		mainLoop mainState state

	_		-> 
	 do
		nState	<- munch input mainState
		mainLoop mainState nState


---------------------------------------------------------------------------------------------------------------------------- munch
munch input	mainState = do
	preludeSource			<- slurpSource filePrelude

	let	source			= "let { " ++ preludeSource ++ "} in print (" ++ input ++ ")"
	let	sState			= compile source mainState

	if msAutoRun mainState 
		then run mainState sState
		else return sState


----------------------------------------------------------------------------------------------------------------------- startState
startState ::	Prog -> Eix ->	MainState	-> State
startState	prog	topEix	mainState
 = State {
	sRun				= True,
	sError				= Nothing,

	sSteps				= 0,
	sReductions			= 0,

	sProg				= prog,
	sHeap				= emptyHeap (msMaxHeapCells mainState),

	sThread				= array (0, msMaxThreads mainState) 
					((0, threadStart topEix) 
						: [(i, threadInactive) | i <- [1 .. msMaxThreads mainState] ]),

	sThreadsActiveRules		= [],

	sThreadsActive			= [],
	sThreadsWaiting			= [],
	sThreadsBlocked			= [],

	sMaxThreadsActive		= (msMaxThreadsActive mainState),
	
	sReduceThread			= (msReduceThread mainState),
	
	sUnblockQueue			= [],
	sUnblockSchedFunc		= (msUnblockSchedFunc 	mainState) (msUnblockDelay mainState),
	
	sSpawnQueue			= [],
	sSpawnSchedFunc			= (msSpawnSchedFunc	mainState) (msUnblockDelay mainState),

	sIOBite				= [],
	sIOAccum			= [],

--- 
	sProf				= (msProf mainState),

	sProfCollections		= 0,
	sProfBlockedSteps		= 0,
	sProfBlockedReductions		= 0,
	sProfThreadsUnblocked		= 0,
	sProfThreadsSpawned		= 0,

	sProfThreadTags			= [],
	sProfHeapCellsUsed		= [],
	sProfThreadsActive		= [],
	sProfThreadsWaiting		= [],
	sProfThreadsBlocked		= [],
	sProfUnblockQueueLength		= [],
	sProfSpawnQueueLength		= []


 }


-------------------------------------------------------------------------------------------------------------------------- compile
compile ::	String ->	MainState	-> State
compile		source		mainState 	=
 let
	tokens0			= lexer source
	(varTable0, tokens)	= stripTable tokens0

	coreVarTable		= zip coreVars (repeat (-1))
	varTable		= coreVarTable ++ varTable0

	parsed			= parse tokens
	
	prog0			= emptyProg (msMaxProgCells mainState)
	(prog1, topEix)		= convertExp varTable prog0  parsed
	prog2			= feedVar prog1 topEix

	nProg			= if msAutoTrimmers mainState then
					buildTrimmers prog2 topEix
				  else	
					prog2
 in
	startState nProg topEix mainState


------------------------------------------------------------------------------------------------------------------------------ run
runStop :: MainState -> State	-> (MainState, State)
runStop mainState state	=
	if msAutoRun mainState then
		(mainState, state)
	else	
		if   (msBreakStep mainState > 0) 
		  && (sSteps  state     == msBreakStep mainState) then
			(mainState { msBreakStep = 0 }, state { sRun = False })

		else 
		 if  (msStepsToGo  mainState > 1) then
			(mainState { msStepsToGo  = msStepsToGo mainState - 1 }, state)

		else
		 if  (msStepsToGo  mainState == 1) then
			(mainState { msStepsToGo  = 0 }, state { sRun = False })

		else
			(mainState, state)
	


run ::	MainState ->	State -> 	IO State
run	mainState	state 		=
  do
	stepState			<- doStep mainState state

	let (nMainState, nState)	= runStop mainState stepState

	if(sRun nState)
	 then	run 	nMainState	nState
	 else
	  do
		putStr 		"\n\n"
		return 		nState


doStep ::	MainState ->	State		-> IO State
doStep		mainState	state	=
 do
	let	nState		= step (msReduceThread mainState) state

	putStr $ sIOBite nState
	hFlush stdout

	if (msTraceLevel mainState > 0)
		then	putStr (take 40 (repeat '-') ++ "\n")
		else	return ()

	if (msTraceLevel mainState >= 1) 
		then	printState nState
		else	return ()

	if (msTraceLevel mainState >= 2)
		then	putStr (heapShow (sHeap nState))
		else	return ()
		
	let stateP	= nState { sIOBite = [] }

	case (sError stateP) of
		Nothing		-> return ()
	 	Just _		-> dumpCrash	stateP

	return	stateP


------------------------------------------------------------------------------------------------------------------------ dumpCrash
dumpCrash	state 	=
 case (sError state) of
  Just (eix, reason) ->
   let
	prog	 	= sProg	state
	cell		= heapGet prog eix
	line		= pcSourceLine cell
   in
	putStr (   "! crash ----------------------------------------\n" 
		++ "line        = " ++ show line		++ "\n"
		++ "eix         = " ++ show eix 		++ "\n"
		++ "exp         = " ++ show (pcExp cell)	++ "\n"
		++ "reason      = " ++ reason			++ "\n"
		++ "\n")
		

------------------------------------------------------------------------------------------------------------------------ dumpState
printState state	= 
 do
	putStr	$ showThreads (sProg state) (sThread state)
	
	putStr	$ showState state

	if (sProf state) 
		then putStr  $ ("---\n" ++ showProf state)
		else return ()

	putStr ("\n")


showState state =
 let
	heap		 = (sHeap state)
	stepsFloat	 = (fromIntegral (sSteps state))  :: Float
	reductionsFloat	 = (fromIntegral (sReductions state)) :: Float
	speedup		 = reductionsFloat / stepsFloat
 in
	   "steps                 = " ++ show (sSteps  state)				++ "\n"
	++ "reductions            = " ++ show (sReductions  state)			++ "\n"
	++ "speedup               = " ++ show (speedup)					++ "\n"
	++ "maxThreadsActive      = " ++ show (sMaxThreadsActive state)  		++ "\n"
	++ "threadsActive         = " ++ show (length (sThreadsActive  state))		++ "\n"
	++ "threadsWaiting        = " ++ show (length (sThreadsWaiting state))		++ "\n"
	++ "threadsBlocked        = " ++ show (length (sThreadsBlocked state))		++ "\n"
	++ "heapCellsUsed         = " ++ show ((hCells heap) - (hFreeCells heap))	++ "\n"
	++ "unblockQueue          = " ++ show (sUnblockQueue state)			++ "\n"
	++ "spawnQueue            = " ++ show (sSpawnQueue   state)  			++ "\n"
	++ "ioBite                = " ++ sIOBite state					++ "\n"
	++ "ioAccum               = " ++ sIOAccum state					++ "\n"

showProf state 
	=  "collections           = " ++ show (sProfCollections	state)			++ "\n"
	++ "profBlockedSteps      = " ++ show (sProfBlockedSteps state)		        ++ "\n"
	++ "profBlockedReductions = " ++ show (sProfBlockedReductions state)            ++ "\n"
	++ "profThreadsUnblocked  = " ++ show (sProfThreadsUnblocked state)             ++ "\n"
	++ "profThreadsSpawned    = " ++ show (sProfThreadsSpawned   state)             ++ "\n"
	
	


showThreads prog threadArray	=
 let
	(low, high)	= bounds threadArray
 in
	concat [showThread prog (threadArray ! threadNum) threadNum | threadNum <- [low .. high]] ++ "\n"


showThread prog thread threadNum =
 case (tControl thread) of 
  (-2)		-> ""
  (-1)		-> showThread_Blocked	prog thread threadNum
  _		-> showThread_Active	prog thread threadNum	


showThread_Active prog thread threadNum
	=	"--- thread " 	    ++ show threadNum						++ "\n"
	++	"eix                   = " ++ show (tControl thread)				++ "\n"
	++	"control               = " ++ show (pcExp $ heapGet prog (tControl thread))	++ "\n"
	++	"env                   = " ++ show (tEnv thread) 				++ "\n"
	++	"stack                 = " ++ show (tStack thread) 				++ "\n"

showThread_Blocked prog thread threadNum
	=	"--- thread " 	    ++ show threadNum						++ "\n"
	++	"eix                   = " ++ "blocked"						++ "\n"
	++	"env                   = " ++ show (tEnv thread) 				++ "\n"
	++	"stack                 = " ++ show (tStack thread) 				++ "\n"









