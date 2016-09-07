{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Deep.Backend.VHDL.Traverse.VHDLM
-- Copyright   :  (c) ES Group, KTH/ICT/ES 2007-2013
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- 'VHDM' (VHDL Monad), related types and functions
--
-----------------------------------------------------------------------------
module ForSyDe.Deep.Backend.VHDL.Traverse.VHDLM where

import ForSyDe.Deep.Backend.VHDL.AST
import qualified ForSyDe.Deep.Backend.VHDL.AST as VHDL
import {-# SOURCE #-} ForSyDe.Deep.Backend.VHDL.GlobalNameTable (globalNameTable)

import ForSyDe.Deep.Ids
import ForSyDe.Deep.ForSyDeErr
import ForSyDe.Deep.System.SysDef (SysDefVal(..))
import ForSyDe.Deep.Netlist.Traverse (TravSEIO)
import ForSyDe.Deep.Process.ProcType (EnumAlgTy(..))

import Data.Data (tyconModule)
import Data.Maybe (fromJust)
import qualified Data.Set as S (filter)
import Data.Set (Set, union, empty, toList)
import Control.Monad.State
import Language.Haskell.TH (nameBase, nameModule, Name, Exp, Arity)
import qualified Language.Haskell.TH as TH
import Data.Typeable (TypeRep,typeRepTyCon,tyConName,tyConPackage,tyConModule)
import Data.Typeable.FSDTypeRepLib

-------------------------------------
-- How does the VHDL Backend work? --
-------------------------------------
-- FIXME: This documentation is a bit outated
-- All the types used in the the System Defintion are translated to VHDL
-- put into Package, the into a Design File and written to disk.
--
-- The System Definition itself is translated to another VHDL Design File and
-- written to disk.
--
-- This Design File will contain only two library units;
-- an Entity Declaration and an Architecture.
-- 1) The Entity Declaration can be obtained from the SysDef directly (without
--    traversing the netlist)
-- 2) The Architecture (or more specifically, its declarations  and
--    the statements) is obtained from the netlist by traversing it.
--
-- The state of the traversal is composed by
--  * list of type defintions translated during the traversal
--  * table of equivalence between Haskell types and the VHDL identifier
--    of its translated type (used to avoid translating the same type
--    multiple times)
--  * the list of declarations of the architecture
--  * the list of statements of the architecture
--  * a table of System Definition references, used to keep track of the
--    system definitions (corresponding to one or more instances in the
--    netlist) whose code was already generated.
--
-- For each process (netlist node) found during the traversal:
--    * A signal declaration is generated for each output and added to
--      the list of architecture declarations.
--    * A VHDL block including the translation of the process is generated
--      and added to the list of architecture statements.
--
-- In the special case of finding a System Instance
--      1) a port map statement is generated and added to the list of
--         architecture statements.
--      2) the System Definition table is used to check if the Design File of
--         the System Definition associated with the instance was written to
--         disk.
--      3) if the the associated System Definition wasn't in the table
--          1) generate and write to disk the corresponding Design File
--          2) add the System Definition to the table

-------------
-- FunTransST
-------------

-- | Function translation state. State used during the translation of
--   ProcFuns to VHDL.
--
-- This type provides the number of fresh names already generated,
-- a translation table from Template Haskell Names to VHDL Expressions
-- (a symbol table) and auxiliary VHDL declarations.
--
-- It only makes sense in a process-function context.
data FunTransST = FunTransST
    {freshNameCount :: Int,
     nameTable      :: [(Name, (Arity, [VHDL.Expr] -> VHDL.Expr ) )],
     -- The table entries work as follows:
     -- (Template Haskell Name (table key),
     --   (Arity, function with which to construct the translated VHDL expression
     --           given itsarguments already translated to VHDL
     -- )
     auxDecs        :: [SubProgDecItem],
     -- Auxiliary VHDL declarations generated during the translation of
     -- the ProcFun to be put in the declaration block of the translated VHDL
     -- function.
     currentFSpec :: Maybe VHDL.SubProgSpec}
     -- The function signature currently being compiled


-- | Initial translation state for functions
initFunTransST :: FunTransST
initFunTransST = FunTransST 0 globalNameTable [] Nothing

-----------
-- VHDLM --
-----------

-- | VHDL backend monad
type VHDLM a = TravSEIO VHDLTravST ContextErr a

----------------
-- VHDLTravST --
----------------

-- | VHDL traversing State. (see 'ForSyDe.Netlist.Traverse.traverseSIO')
data VHDLTravST = VHDLTravST
  {local  :: LocalVHDLST, -- Local State (related to the system currently
                          -- compiled)
   global :: GlobalVHDLST}  -- Global state (related to all systems being
                            -- recursively compiled)

data LocalVHDLST = LocalVHDLST
   {currSysDef     :: SysDefVal, -- System definition which is currently
                                 -- being compiled
   context         :: Context,  -- Error Context
   funTransST      :: FunTransST,  -- Translation state for functions (ProcFuns)
                                   -- It only makes sense
                                   -- in a process-function context
   localRes        :: LocalTravResult} -- Result accumulated during the
                                       -- traversal of current System Definition
                                       -- netlist




-- | initialize the local state
initLocalST :: SysDefVal -> LocalVHDLST
initLocalST sysDefVal =
 LocalVHDLST sysDefVal (SysDefC (sid sysDefVal) (loc sysDefVal))
             initFunTransST emptyLocalTravResult

-- | Execute certain operation with a concrete local state.
--   The initial local state is restored after the operation is executed
withLocalST :: LocalVHDLST -> VHDLM a -> VHDLM a
withLocalST l' action =  do
  -- get the initial local state
  st <- get
  let l = local st
  -- set the modified state
  put st{local=l'}
  -- execute the action
  res <- action
  -- restore the initial local state
  st' <- get
  put st'{local=l}
  -- return the result
  return res

-- | Execute certain operation with the initial function translation state
--   The initial state is restored after the operation is executed
withInitFunTransST :: VHDLM a -> VHDLM a
withInitFunTransST action = do
  -- get the initial name space
  st <- get
  let l = local st
      ns = funTransST l
  -- set the empty name space
  put st{local=l{funTransST=initFunTransST}}
  -- execute the action
  res <- action
  -- restore the initial name table
  st' <- get
  let l' = local st'
  put st'{local=l'{funTransST=ns}}
  -- return the result
  return res



data GlobalVHDLST = GlobalVHDLST
  {globalSysDef :: SysDefVal, -- global system definition
                              -- (the first-level system being compiled)
   ops          :: VHDLOps,  -- Compilation options
   globalRes    :: GlobalTravResult, -- Result accumulated during the
                                     -- whole compilation
   enumTypes    :: Set EnumAlgTy, -- Set of the enumerated
                                  -- algebraic types accumulated
                                  -- by all ProcFuns and ProcVals
                                  -- in the system
   typeTable    :: [(FSDTypeRep, TypeMark)],  -- Type translation table
   transUnconsFSVecs :: [FSDTypeRep]} -- Unconstrained FSVecs previously translated.
                                   -- Each unconstrained FSVec is represented by
                                   -- the 'TypeRep' of its elements


-- | Empty initial traversing state
initGlobalVHDLST :: SysDefVal -> GlobalVHDLST
initGlobalVHDLST  sysDefVal =
 GlobalVHDLST sysDefVal defaultVHDLOps emptyGlobalTravResult empty [] []

-- | Empty initial traversing state
initVHDLTravST :: SysDefVal -> VHDLTravST
initVHDLTravST sysDefVal =
 VHDLTravST (initLocalST sysDefVal) (initGlobalVHDLST sysDefVal)

-------------
-- TravResult
-------------

-- | Local result accumulated during the traversal of a netlist
data LocalTravResult = LocalTravResult
  {archDecs  :: [BlockDecItem], -- generated architecture declarations
   archSms   :: [ConcSm]      } -- generated architecture statements



-- | empty local VHDL compilation result
emptyLocalTravResult :: LocalTravResult
emptyLocalTravResult = LocalTravResult [] []


-- | Global Results accumulated throughout the whole compilation
data GlobalTravResult = GlobalTravResult
 {typeDecs      :: [TypeDec], -- Types translated during the traversal
  subtypeDecs   :: [SubtypeDec], -- Subtypes translated during the traversal
  subProgBodies :: [SubProgBody] } -- Functions or procedures generated during
                                   -- the traversal



-- | empty global VHDL compilation result
emptyGlobalTravResult :: GlobalTravResult
emptyGlobalTravResult = GlobalTravResult [] [] []


----------
-- VHDLOps
----------

-- | VHDL Compilation options
data VHDLOps = VHDLOps {debugVHDL :: VHDLDebugLevel, -- ^ Debug mode
                        recursivityVHDL :: VHDLRecursivity,
                        execQuartus  :: Maybe QuartusOps, -- ^ Analyze the generated code with Quartus
                        compileModelsim :: Bool -- ^ Compile the generated code with Modelsim
                                        }
 deriving (Eq, Show)

-- | Debug level
data VHDLDebugLevel = VHDLNormal | VHDLVerbose
 deriving (Eq, Ord, Show)

-- | Print a message to stdout if in verbose mode
debugMsg :: String -> VHDLM ()
debugMsg str = do
 debugLevel <- gets (debugVHDL.ops.global)
 when (debugLevel > VHDLNormal)
      (liftIO $ putStr ("DEBUG: " ++ str))

-- | Recursivity, should the parent systems of system instances be compiled as
--   well?
data VHDLRecursivity = VHDLRecursive | VHDLNonRecursive
 deriving (Eq, Show)

-------------
-- QuartusOps
-------------

-- Quartus options

-- | Options passed to Quartus II by the VHDL Backend. Most of them are optional
--   and Quartus will use a default value.
--
--   It contains:
--
--     * What action to perform
--
--     * Optinally, the minimum acceptable clock frequency (fMax) expressed in MHz
--
--     * FPGA family and specific device model (both are independently optional).
--
--     * Pin assignments, in the form (VHDL Pin, FPGA Pin). Note
--       that Quartus will automatically split composite VHDL ports
---      (arrays and records) in various pins with different logical names.
data QuartusOps =
     QuartusOps {action :: QuartusAction,
                 fMax   :: Maybe Int,
                 fpgaFamiliyDevice :: Maybe (String, Maybe String),
                 pinAssigs :: [(String,String)] }
 deriving (Eq, Show)

-- | Action to perform by Quartus
data QuartusAction = AnalysisAndElaboration  -- ^ Analysis and eleboration flow
                   | AnalysisAndSynthesis -- ^ Call map executable
                   | FullCompilation -- ^ Compile flow
 deriving (Eq, Show)

-- | Options to check if the model is synthesizable, all options except
--   the action to take are set to default.
checkSynthesisQuartus :: QuartusOps
checkSynthesisQuartus = QuartusOps AnalysisAndSynthesis Nothing Nothing []


-- | Check if we are in recursive mode
isRecursiveSet :: VHDLM Bool
isRecursiveSet = do
  recOp <- gets (recursivityVHDL.ops.global)
  return $ recOp == VHDLRecursive

-- | Check if we want to compile the results with modelsim
isCompileModelsimSet :: VHDLM Bool
isCompileModelsimSet = gets (compileModelsim.ops.global)

-- | Default traversing options
defaultVHDLOps :: VHDLOps
defaultVHDLOps =  VHDLOps VHDLNormal VHDLRecursive Nothing False


-- | Set VHDL options inside the VHDL monad
setVHDLOps :: VHDLOps -> VHDLM ()
setVHDLOps options =  modify (\st -> st{global=(global st){ops=options}})


-------------------------------------
-- Useful functions in the VHDL Monad
-------------------------------------


-- | Add a signal declaration to the 'LocalTravResult' in the State
addSigDec :: SigDec -> VHDLM ()
addSigDec dec = modify addFun
 -- FIXME: use a queue for the declarations
  where addFun st = st{local=l{localRes=r{archDecs=ads ++ [BDISD dec]}}}
         where l  = local st
               r  = localRes l
               ads = archDecs r


-- | Add a statement to the 'LocalTravResult' in the State
addStm :: ConcSm -> VHDLM ()
addStm sm = modify addFun
 -- FIXME: use a queue for the statements
  where addFun st = st{local=l{localRes=r{archSms=aSms ++ [sm]}}}
         where l  = local st
               r  = localRes l
               aSms = archSms r


-- | Find a previously translated custom type
lookupCustomType :: FSDTypeRep -> VHDLM (Maybe SimpleName)
lookupCustomType rep = do
 transTable <- gets (typeTable.global)
 let res = lookup rep transTable
 return res
 --when (res==Nothing) $ mapM_ (\(elem,_) -> liftIO $ putStrLn $ (tyConName.typeRepTyCon) elem ++ "," ++ (tyConModule.typeRepTyCon) elem ++ "," ++(tyConPackage.typeRepTyCon) elem) transTable
 --when (res==Nothing) (liftIO $ putStrLn $ (tyConName.typeRepTyCon) rep ++ "," ++ (tyConModule.typeRepTyCon) rep ++ "," ++(tyConPackage.typeRepTyCon) rep ++ "\n")
 --let gkey = typeRepKey rep
 --mapM_ (\(elem,_) -> let key = typeRepKey elem in liftIO $ putStrLn $ show (gkey==key) ++ ".." ++(tyConPackage.typeRepTyCon) elem ++ ".." ++ (tyConModule.typeRepTyCon) elem ++ ".." ++(tyConName.typeRepTyCon) elem) transTable

 --liftIO$putStrLn$ "\n" ++ (tyConPackage.typeRepTyCon) rep ++ ".." ++ (tyConModule.typeRepTyCon) rep ++ ".." ++(tyConName.typeRepTyCon) rep

 --liftIO$putStrLn$show (lookup rep transTable)  ++ "\n\n"

 --return $ lookup rep transTable


-- | Add enumerated types to the global state
addEnumTypes :: Set EnumAlgTy -> VHDLM ()
addEnumTypes newETs = do
 globalST <- gets global
 let oldETs = enumTypes globalST
 modify (\st -> st{global = globalST {enumTypes = oldETs `union` newETs}})

-- | Check if a Template haskell 'Name' corresponding to
--   a Enumerated-type data constructor is present in the enumerated
--   types accumulated in the global state and return the corresponding
--   VHDL identifier.
getEnumConsId :: Name -> VHDLM (Maybe VHDLId)
getEnumConsId consName = do
 let consModule = (fromJust.nameModule) consName
     consBase = nameBase consName
 enums <- gets (enumTypes.global)
 let matchesName (EnumAlgTy dataName enums) =
                 tyconModule dataName == consModule && elem consBase enums
 case (toList.(S.filter matchesName)) enums of
   []  -> return Nothing
   [_] -> liftM Just (liftEProne $ mkVHDLExtId consBase)
   -- _ -> this shouldn't happen since the enumerated types stored are unique
   _ ->  intError "ForSyDe.Backend.VHDL.Traverse.VHDLM.getEnum"
          (UntranslatableVHDLFun $ GeneralErr (Other "pattern match inconsistency"))


-- | Add a cutom type to the global results and type translation table
addCustomType :: FSDTypeRep -> Either TypeDec SubtypeDec -> VHDLM ()
addCustomType rep eTD = do
 globalST <- gets global
 let transTable = typeTable globalST
     gRes = globalRes globalST
     tDecs =  typeDecs gRes
     stDecs = subtypeDecs gRes
 -- FIXME: use queues
 modify (\st -> st{global =
                    case eTD of
                     Left td@(TypeDec id _) ->
                       if id `notElem` (map snd transTable) then
                        globalST
                             {typeTable = transTable ++ [(rep, id)],
                              globalRes = gRes{typeDecs = tDecs ++ [td]}}
                       else globalST
                     Right std@(SubtypeDec id _) ->
                       if id `notElem` (map snd transTable) then
                        globalST
                             {typeTable = transTable ++ [(rep, id)],
                              globalRes = gRes{subtypeDecs = stDecs ++ [std]}}
                       else globalST
                   })

-- | Add type declaration to the global results
addTypeDec :: TypeDec  -> VHDLM ()
addTypeDec td = do
 globalST <- gets global
 let gRes = globalRes globalST
     tDecs =  typeDecs gRes
 -- FIXME: use queues
 modify (\st -> st{global = globalST{globalRes = gRes{typeDecs = tDecs ++ [td]}}})


-- | Add subtype declaration to the global results
addSubtypeDec :: SubtypeDec  -> VHDLM ()
addSubtypeDec std = do
 globalST <- gets global
 let gRes = globalRes globalST
     stDecs =  subtypeDecs gRes
 -- FIXME: use queues
 modify (\st -> st{global = globalST{
                              globalRes = gRes{subtypeDecs = stDecs ++ [std]}}})


-- | Add an unconstrained FSVec to the global results
addUnconsFSVec :: FSDTypeRep -> VHDLM ()
addUnconsFSVec trep = do
 globalST <- gets global
 -- FIXME: use queues
 modify (\st -> st{global =
                    globalST{
                     transUnconsFSVecs = (transUnconsFSVecs globalST) ++ [trep]}})

-- | Add a subprogram to the global results
addSubProgBody :: SubProgBody -> VHDLM ()
addSubProgBody newBody = do
 globalST <- gets global
 let gRes = globalRes globalST
     bodies = subProgBodies gRes
 -- FIXME: use queues
 modify (\st -> st{global = globalST
                       {globalRes = gRes{subProgBodies = bodies ++ [newBody]}}})


-- | Add a TH-name (arity, VHDL expression construtor function)  pair to the translation namespace table
addTransNamePair :: Name -> Arity -> ([Expr] -> Expr) -> VHDLM ()
addTransNamePair thName arity vHDLFun = do
 lState <- gets local
 let s = funTransST lState
     table = nameTable s
 modify (\st -> st{local=lState{funTransST=s{
                                       nameTable=(thName,(arity,vHDLFun)):table}}})

-- | Add a declarations to Auxiliary VHDL declarations of the Function
--   translation state
addDecsToFunTransST :: [SubProgDecItem] -> VHDLM ()
addDecsToFunTransST decs = do
 lState <- gets local
 let s = funTransST lState
     auxs = auxDecs s
 modify (\st -> st{local=lState{funTransST=s{
                                       auxDecs=decs++auxs}}})



getCurrentFunctionSpec :: VHDLM SubProgSpec
getCurrentFunctionSpec = do 
   fSpecM <- gets (currentFSpec.funTransST.local)
   case fSpecM of
        Just fSpec -> return fSpec
        Nothing    -> error "Bug: Incomplete translation context, found `Nothing` expected `Just SubProgSpec`"

putCurrentFunctionSpec :: SubProgSpec -> VHDLM ()
putCurrentFunctionSpec spec = do
 lState <- gets local
 let s = funTransST lState
 modify (\st -> st{local=lState{funTransST=s{
                                       currentFSpec=Just spec}}})

-- | Get a fresh VHDL Identifier and increment the
--   tranlation-namespace-count of freshly generated identifiers.
--
--   Note that all user-supplied identifiers (process ids, port ids,
--   and function parameters) are translated to extended VHDL
--   identifiers. That, together with the fact that basic and extended
--   VHDL identifers live in different namespaces, guarantees that
--   freshly generated basic VHDL identifiers cannot clash with the
--   ones supplied by the frontend.
genFreshVHDLId :: VHDLM VHDLId
genFreshVHDLId = genVHDLId "fresh_"

genVHDLId :: String -> VHDLM VHDLId
genVHDLId prefix = do
 lState <- gets local
 let ns = funTransST lState
     count = freshNameCount ns
 modify (\st -> st{local=lState{funTransST=ns{freshNameCount=count+1}}})
 liftEProne.mkVHDLBasicId $ (prefix ++ show count)

-- | Lift an 'EProne' value to the VHDL monad setting current error context
--   for the error
-- liftEProne :: EProne a -> VHDLM a
liftEProne :: EProne a -> VHDLM a
liftEProne ep = do
 cxt <- gets (context.local)
 either (throwError.(ContextErr cxt)) return ep

-- | Throw a ForSyDe error, setting current error context
throwFError :: ForSyDeErr -> VHDLM a
throwFError = liftEProne.Left



-- | Execute certain operation with a concrete process context.
--   The initial context is restored after the operation is executed
--   Note: the initial context must be a system context or 'InconsistenContexts'
--         will be raised.
withProcC :: ProcId -> VHDLM a -> VHDLM a
withProcC pid action = do
  -- get the initial context
  st <- get
  let l = local st
      c = context l
  -- set the modified name context
  put st{local=l{context=setProcC pid c}}
  -- execute the action
  res <- action
  -- restore the initial name context
  st' <- get
  let l' = local st'
  put st'{local=l'{context=c}}
  -- return the result
  return res



-- | Execute certain operation with a concrete process function context.
--   The initial context is restored after the operation is executed
--   Note: the initial context must be a process context or
--         'InconsistenContexts' will be raised.
withProcFunC :: Name -> Loc -> VHDLM a -> VHDLM a
withProcFunC name loc action = do
  -- get the initial context
  st <- get
  let l = local st
      c = context l
  -- set the modified context
  put st{local=l{context=setProcFunC name loc c}}
  -- execute the action
  res <- action
  -- restore the initial context
  st' <- get
  let l' = local st'
  put st'{local=l'{context=c}}
  -- return the result
  return res



-- | Execute certain operation with a concrete process function context.
--   The initial context is restored after the operation is executed
--   Note: the initial context must be a process context or
--         'InconsistenContexts' will be raised.
withProcValC :: Exp -> VHDLM a -> VHDLM a
withProcValC exp action = do
  -- get the initial context
  st <- get
  let l = local st
      c = context l
  -- set the modified context
  put st{local=l{context=setProcValC exp c}}
  -- execute the action
  res <- action
  -- restore the initial context
  st' <- get
  let l' = local st'
  put st'{local=l'{context=c}}
  -- return the result
  return res


----------------
-- IntSignalInfo
----------------

-- | Intermediate signal information. Tag generated for each output of each
--   node found during the traversal.
-- (see ForSyDe.Netlist.Traverse.traverseSIO).
--   It contains the VHDL intemediate signal name associated with the process
--   output.
type IntSignalInfo = SimpleName

