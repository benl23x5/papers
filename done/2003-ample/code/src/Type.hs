
module	Type ( 
	Name, 		Eix,	Vix,	Env,	Trimmer,	ThreadNum,
	Exp(..),	
	Constant(..), 
	StackElem(..),	Stack,
	ProgCell(..), 	Prog,
	Cell(..),
	State(..),	
	Thread(..),	
	ThreadTag(..),
	ActivationFunction,	
	ActivationRecord,
	ReduceMod(..)
)
where

import	Array
import	Heap


----------------------------------------------------------------------------------------------------------------------- Exp
type	Name	= String
type	Eix	= Int				-- expression index
type	Vix	= Int				-- variable index
type	Env	= [Vix]				-- environment
type	Trimmer	= [Vix]

data Exp
	= Lambda	Eix	
	| ExpVar	Eix			Vix
	| Var		Vix
	| Let 		[(Eix, Trimmer)]	(Eix, Trimmer)
	| Case		(Eix, Trimmer)		[ ((Name, Int), (Eix, Trimmer)) ]
	| Construct	Name			Int
	| Constant	Constant
	| PrimFunc	Name
	| Seq		Eix			Eix
	| Par		Vix			Eix
 deriving Show


data Constant
	= CInt		Int
	| CFloat	Double
	| CChar		Char
 deriving Show



---------------------------------------------------------------------------------------------------------------------- Stack
data StackElem
	= SEPointer		Vix
	| SEUpdate		Vix
	| SEAlts		( [ ((Name, Int), (Eix, Trimmer)) ], Env )
	| SESeq			( Eix, Env )
 deriving Show

type	Stack	= [StackElem]


---------------------------------------------------------------------------------------------------------------------- Prog
data ProgCell
 = ProgCell {
	pcExp		:: Exp,
	pcSourceLine	:: Int,
	pcDepth		:: Int
 }
 deriving Show

type Prog	= Heap ProgCell

---------------------------------------------------------------------------------------------------------------------- Heap
data Cell
 = Cell {
	cMark		:: Bool,
	cEix		:: Eix,
	cEnv		:: Env,
	cBlocks		:: [Int]
 }
 deriving Show


---------------------------------------------------------------------------------------------------------------------- State
type ThreadNum	= Int
type Age	= Int

data State
 = State {
	sRun				:: Bool,
	sError				:: Maybe (Eix, String),

	sSteps 				:: Int,
	sReductions			:: Int,

	sProg				:: Heap ProgCell,
	sHeap				:: Heap Cell,

	sThread				:: Array Int Thread,
	sThreadsActive			:: [ThreadNum],
	sThreadsActiveRules		:: [String],		-- name of rule that was executed, assigned during step

	sThreadsWaiting			:: [ThreadNum],
	sThreadsBlocked			:: [ThreadNum],

	sMaxThreadsActive		:: Int,

	sReduceThread			:: State -> ThreadNum -> (State, ([ReduceMod], String)),

	sUnblockQueue			:: [(Int, (ThreadNum, (Eix, Env)))],
	sUnblockSchedFunc		:: forall a. [a] -> [(Int, a)] -> [(Int, a)],

	sSpawnQueue			:: [(Int, (Eix, Env, ThreadNum))],
	sSpawnSchedFunc			:: forall a. [a] -> [(Int, a)] -> [(Int, a)],

	sIOBite				:: String,
	sIOAccum			:: String,

---
	sProf				:: Bool,

	sProfCollections		:: Int,
	sProfBlockedSteps		:: Int,
	sProfBlockedReductions		:: Int,
	sProfThreadsSpawned		:: Int,
	sProfThreadsUnblocked		:: Int,

	sProfThreadTags			:: [[(ThreadNum, ThreadTag)]],
	sProfHeapCellsUsed		:: [Int],

	sProfThreadsActive		:: [Int],
	sProfThreadsWaiting		:: [Int],
	sProfThreadsBlocked		:: [Int],

	sProfUnblockQueueLength		:: [Int],
	sProfSpawnQueueLength		:: [Int]
 }

type ActivationFunction	=	
	Int -> Int -> 		[Int]	-> [Int]
-- 	delay  timeSinceLast	ages	=  ixToActivate


type ActivationRecord	 = 
	(Int, (Int, 		(Eix, Env)))
--	(Age, (ThreadNum,	(Eix, Env)))


----------------------------------------------------------------------------------------------------------------------------
data ThreadTag 
	= ThreadActive 	String
	| ThreadWaiting
	| ThreadBlocked	
	| ThreadInactive
 deriving (Show, Eq)


--------------------------------------------------------------------------------------------------------------------- Thread
data Thread
 = Thread {
	tControl		:: Eix,
	tEnv			:: [Int],
	tStack			:: [StackElem]
 }
 deriving Show




--------------------------------------------------------------------------------------------------------------------- ReduceMod
data ReduceMod 
	= Error		Eix	String
	| Stop
	| Print		String

	| Update	Int	Cell
	| Remove	Int
	| Unblock	Int	Eix	Env
	| Spark		Int	Eix	Env
	| BlockOn	Int	Int
 deriving Show


