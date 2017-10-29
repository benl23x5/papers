
module Machine.Execute.Shake where
import Machine.Execute.Inject
import Machine.Execute.Eval
import Machine.Pretty
import Machine.Base
import Data.Map                         (Map)
import Data.List
import qualified Data.Map.Strict        as Map
import Text.PrettyPrint.Leijen

-- | An output action from a Shake evaluation.
data Action
        = ActionPush Channel Value
        deriving Show

instance Pretty Action where
 pretty (ActionPush c v)
  = parens $ text "push" <+> pretty c <+> pretty v

---------------------------------------------------------------------------------------------------
shakeSteps 
        :: [Process]            -- ^ Processes which are currently stalled.
        -> [Process]            -- ^ Processes we can still try to execute.
        -> [Action]             -- ^ Actions produced so far.
        -> ( [Process]          --   Result processes.
           , [Action])          --   Result actions.

shakeSteps stalled [] acc
 = (stalled, acc)

shakeSteps stalled (p : psRest) acc
 = case shakeStep p of
        Just (p', mAction)
         -> case mAction of
                Nothing
                 -> shakeSteps [] (stalled ++ (p' : psRest)) acc

                Just action@(ActionPush channel value)
                 -- The process wants to push to one of its output channels,
                 -- so the consumers need to be ready to accept it for the step to complete.
                 |  Just stalled' <- injects stalled channel value
                 ,  Just psRest'  <- injects psRest  channel value
                 -> shakeSteps [] (stalled' ++ (p' : psRest')) (acc ++ [action])

                 -- One of the consumers wasn't ready to accept the output.
                 |  otherwise
                 -> shakeSteps (stalled ++ [p]) psRest acc

        Nothing
         -> shakeSteps (stalled ++ [p]) psRest acc


---------------------------------------------------------------------------------------------------
shakeStep :: Process -> Maybe (Process, Maybe Action)
shakeStep p
 |  Just instr  <- lookup (processLabel p) (processBlocks p)
 =  case shake instr (processIns p) (processHeap p) of
        Nothing 
         -> Nothing

        Just (label', sIns', heap', mAction)
         -> Just ( p    { processLabel  = label'
                        , processIns    = sIns'
                        , processHeap   = heap' }
                 , mAction)


---------------------------------------------------------------------------------------------------
-- | Shake an instruction of a process.
shake   :: Instruction                          -- ^ Instruction to shake.
        -> Map Channel InputState               -- ^ States of input channels.
        -> Heap                                 -- ^ Current heap.
        -> Maybe ( Label                        --   New label to jump to.
                 , Map Channel InputState       --   New channel input states.
                 , Heap                         --   New heap.
                 , Maybe Action)                --   Output action.

shake instr sInput heap@(Heap hvHeap)
 = case instr of
        -- Pull a value from a channel.
        Pull channel var (Next label' hxUpdate)
         -- If there is a pending value then write it to the heap,
         -- and set the channel state to 'Have' to indicate we're currently using it.
         |   Just (Pending value) <- Map.lookup channel sInput
         ->  Just ( label'
                 , Map.insert channel Have sInput
                 , Heap (Map.unions 
                          [ Map.singleton var value
                          , Map.map (eval heap) hxUpdate
                          , hvHeap ])
                 , Nothing)

         |  otherwise
         -> Nothing


        -- Drop an element that we currently have.
        Drop channel (Next label' hxUpdate) 
         -- If the channel is in the 'Have' state then we've already loaded its
         -- element into the heap. Set the state to None so we can load the next element.
         |  Just Have   <- Map.lookup channel sInput
         -> Just ( label'
                 , Map.insert channel None sInput
                 , Heap (Map.unions
                          [ Map.map (eval heap) hxUpdate
                          , hvHeap ])
                 , Nothing)


        -- Push an element to an output stream.
        Push channel expr (Next label' hxUpdate)
         -> Just ( label'
                 , sInput
                 , Heap (Map.unions
                          [ Map.map (eval heap) hxUpdate
                          , hvHeap ])
                 , Just (ActionPush channel (eval heap expr)))


        -- Jump to a new process label.
        Jump (Next label' hxUpdate)
         -> Just ( label'
                 , sInput
                 , Heap (Map.unions
                          [ Map.map (eval heap) hxUpdate
                          , hvHeap ])
                 , Nothing)


        -- Case branching.
        Case xScrut (Next labelThen hxThen) (Next labelElse hxElse)
         -> case eval heap xScrut of
             VBool True    
              -> Just   ( labelThen
                        , sInput
                        , Heap (Map.unions
                                 [ Map.map (eval heap) hxThen
                                 , hvHeap ])
                        , Nothing )

             VBool False
              -> Just   ( labelElse
                        , sInput
                        , Heap (Map.unions
                                 [ Map.map (eval heap) hxElse
                                 , hvHeap ])
                        , Nothing )

             _ -> error "shake case: type error"

        _ -> Nothing


