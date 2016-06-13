----------------------------------------------------------------------------------------------------------------------------- Step
module Step (
	step
)

where

import	Array
import	List
import	Util

import	Type
import	Profile
import	Heap
import	Garbage
import	Schedule
import	ReduceThread
import	Init

----------------------------------------------------------------------------------------------------------------------------- step
step	:: (State -> Int -> (State, ([ReduceMod], String)))
	-> State		
	-> State

step	reduceThread startState
	= incSteps 
	$ runStop 
	$ collect
	$ ageSpawnQueue
	$ ageUnblockQueue
	$ spawnThreads
	$ unblockThreads
	$ (\state -> if (sProf state) then profileAccumulatePostReduce state else state)
	$ (reduceThreads reduceThread) 
	$ (\state -> if (sProf state) then profileAccumulatePreReduce  state else state)
	$ assignActiveThreads
	$ startState

------------------------------------------------------------------------------------------------------------------------- incsteps
incSteps :: 	State -> State
incSteps 	state = state { sSteps = (sSteps state) + 1 }


-------------------------------------------------------------------------------------------------------------------------- runStop
runStop	::	State -> State
runStop 	state =	
 let
	runnableThreads		= map fst $ filter (\x -> threadIsRunnable (snd x)) $ assocs (sThread state)
 in
	if  (runnableThreads == []) 
	 && (sUnblockQueue state == [])
	 && (sSpawnQueue   state == [])

		then state { sRun = False }
		else state


-------------------------------------------------------------------------------------------------------------------------- collect
collect	:: 	State -> State
collect	  	state	=  
	if (hFreeCells (sHeap state)) < 100 then
	  let	stateGC = garbageCollect state
	  in	stateGC { sProfCollections	= (sProfCollections stateGC) + 1 }
	 else	
		state


-------------------------------------------------------------------------------------------------------------------- ageSpawnQueue
ageSpawnQueue ::   State -> State
ageSpawnQueue 	   state =
	state { sSpawnQueue   = ageQueue (sSpawnQueue state) }



------------------------------------------------------------------------------------------------------------------ ageUnblockQueue
ageUnblockQueue :: State -> State
ageUnblockQueue    state =
	state { sUnblockQueue = ageQueue (sUnblockQueue state) }


ageQueue ::	[(Int, a)] -> [(Int, a)]
ageQueue	queue	   =
 let
	(ages, a)	= unzip queue
	ages'		= map (\x -> if x > 0 then x - 1 else 0) ages
 in
	zip ages' a


--------------------------------------------------------------------------------------------------------------------- spawnThreads
spawnThreads ::	  State -> State
spawnThreads	  state =
 let
	(schedSpawnNow, schedSpawnLater)	= partition (\x -> fst x == 0) (sSpawnQueue state)
	spawnNow				= map snd schedSpawnNow
 in
	(chain spawnThread state spawnNow) {
		sSpawnQueue		= schedSpawnLater
	}

spawnThread ::	State	-> (Eix, Env, Int)	-> State
spawnThread	state	   (eix, env, p)	=
 let
	threadNum	= firstInactiveThread (sThread state)
	nThread		= threadRunnable eix env [SEUpdate p]
 in
	state { sThread			= (sThread state) // [(threadNum, nThread)],
		sProfThreadsSpawned 	= (sProfThreadsSpawned state) + 1 } 



------------------------------------------------------------------------------------------------------------------- unblockThreads
unblockThreads :: State -> State
unblockThreads    state =
 let
	(schedUnblockNow, schedUnblockLater)	= partition (\x -> fst x == 0) (sUnblockQueue state)
	unblockNow				= map snd schedUnblockNow
 in
	(chain unblockThread state unblockNow) {
		sUnblockQueue		= schedUnblockLater
	}



unblockThread :: State ->  (Int,       (Eix, Env)) 	-> 	State
unblockThread 	 state 	   (threadNum, (eix, env))	=
 let
	nThread	= ((sThread state) ! threadNum) { tControl = eix, tEnv = env }
 in
	state {	
		sThread			= (sThread state) // [(threadNum, nThread)],
		sProfThreadsUnblocked	= (sProfThreadsUnblocked state) + 1
	}


-------------------------------------------------------------------------------------------------------------------- reduceThreads
reduceThreads
	:: (State -> Int -> (State, ([ReduceMod], String)))
	-> State -> State

reduceThreads reduceThread 	state =
 let
	(state1, modRules)		= chainR reduceThread state (sThreadsActive state)
	(modss, rules)			= unzip modRules

	mods				= concat modss

	(updateMods,   otherMods1)	= partition reduceModIsUpdate	mods
	(blockMods,    otherMods2)	= partition reduceModIsBlockOn	otherMods1
	(unblockMods,  otherMods3)	= partition reduceModIsUnblock	otherMods2
	(spawnMods,    otherMods4)	= partition reduceModIsSpark	otherMods3
	
	state2				= chain applyMod state1 updateMods
	state3				= chain applyMod state2 blockMods
	state4				= chain applyMod state3 unblockMods
	state5				= chain applyMod state4 spawnMods
	state6				= chain applyMod state5 otherMods4
 in
	state6 {
		sThreadsActiveRules	= rules
	}


-------------------------------------------------------------------------------------------------------------- assignActiveThreads
assignActiveThreads :: 	State 	-> State
assignActiveThreads	state	=
 let
	threadNumsRunnable		= map fst $ filter (\x -> threadIsRunnable (snd x)) $ assocs (sThread state)
	threadNumsBlocked		= map fst $ filter (\x -> threadIsBlocked  (snd x)) $ assocs (sThread state)

	(threadNumsActive, threadNumsWaiting)	
					= splitAt (sMaxThreadsActive state) threadNumsRunnable
 in
	state { 
		sThreadsActive	= threadNumsActive, 
		sThreadsWaiting	= threadNumsWaiting,
		sThreadsBlocked	= threadNumsBlocked }




