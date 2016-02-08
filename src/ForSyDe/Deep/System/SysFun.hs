{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, MultiParamTypeClasses,
             FunctionalDependencies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.System.SysFun
-- Copyright   :  (c) ES Group, KTH/ICT/ES 2007-2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides the 'SysFun' class and a Template Haskell 
-- function to check whether a 'Type' complies with the expected 
-- type of a system function not.
--
----------------------------------------------------------------------------- 
module ForSyDe.Deep.System.SysFun 
 (SysFun(..),
  SysFunToSimFun(..), 
  SysFunToIOSimFun(..),
  funOutInstances, 
  checkSysFType) where

import ForSyDe.Deep.Ids(PortId)
import ForSyDe.Deep.Netlist (NlSignal, Signal(..))
import ForSyDe.Deep.ForSyDeErr
import ForSyDe.Deep.Process.ProcType (ProcType(..))

import Language.Haskell.TH.TypeLib

import Data.Dynamic
import Text.Regex.Posix ((=~))
import qualified Language.Haskell.TH as TH (Exp)
import Language.Haskell.TH
import Text.ParserCombinators.ReadP (readP_to_S)
import Data.Typeable.FSDTypeRepLib

---------------
-- SysFun class
---------------

-- | Class used to describe a System function. It uses the same trick
--   as 'Text.Printf' to implement the variable number of arguments.
class SysFun f where
 -- | Gets the result of applying a system function to input port signals
 --   whose name is given in first argument.
 --   In case the input id list is not long enough, \"default\" will be used
 --   as the id of the remaining ports.
 applySysFun :: f 
             -> [PortId] -- ^ ids used to build the input ports
             -> ([NlSignal], [FSDTypeRep], [FSDTypeRep])
               -- ^ (output signals returned by the system function,
               --    types of input ports, 
               --    types of output ports)

 -- | Transforms a primitive-signal-list version of a system function
 --   into a standard system function.
 --   Note the length of input/output lists of the list version must match with 
 --   the argument number and return tuple size of the system function.
 fromListSysFun :: ([NlSignal] -> [NlSignal]) -- ^ primitive-signal-list sysfun
                -> [NlSignal] -- ^ accumulated primitive signals
                              --   (must be initialized to [])
                -> f
                   


-----------------------
-- SysFunToSimFun class
-----------------------

-- | Multiparameter class to transform a System Function into a Simulation 
--   Function, able to simulate a System using a list-based representation 
--   of its signals.
class SysFun sysFun => 
      SysFunToSimFun sysFun simFun | sysFun -> simFun, simFun -> sysFun where
 -- | Transforms a dynamic-list version of a simulation function
 --   into a standard simulation function.
 --   Note the length of input/output lists of the list version must match with 
 --   the argument number and return tuple size of the simulation function.
 fromDynSimFun :: ([[Dynamic]] -> [[Dynamic]]) -- ^ dynamic-list simfun
                -> [[Dynamic]] -- ^ accumulated dynamic values
                               --   (must be initialized to [])
                -> simFun

-------------------------
-- SysFunToIOSimFun class
-------------------------

-- | Multiparameter class to transform a System Function into an IO 
--   Simulation Function, able to externally simulate a System using a 
--   list-based representation of its signals.
class SysFun sysFun => 
      SysFunToIOSimFun sysFun simFun | 
      sysFun -> simFun, simFun -> sysFun where
 -- | Transforms a TH/String version of a simulation function into a
 --   standard simulation function.  
 --   Again, the length of input/output lists of the list version must
 --   match with the argument number and return tuple size of the
 --   simulation function.
 fromTHStrSimFun :: ([[TH.Exp]] -> IO [[String]]) -- ^ TH/String-list simfun
                 -> [[TH.Exp]] -- ^ accumulated dynamic values
                               --   (must be initialized to [])
                 -> simFun

-- Function to automatically generate instances for the system and
-- simulate function outputs with Template Haskell. For example, in
-- the case of 2 outputs, the code generated would be:
--
-- @
--   NOTE: even if all ProcType constraints could be less restrictive (Typeable
--         would do), it makes more sense, and who nows, maybe we end up requiring
--         full ProcType functionality at some point. 
--
--
-- instance (ProcType o1, ProcType o2) => SysFun (Signal o1, Signal o2) where
--  applyFun (o1, o2) _ = 
--         ([unSignal o1, unSignal o2], [], [typeOf o1, typeOf o2]) 
--  fromListSysFun f accum = (Signal o1, Signal o2)
--          where [o1, o2] = f (reverse accum) 
--
-- instance (ProcType o1, ProcType o2) => 
--          SysFun2SimFun (Signal o1, Signal o2) ([o1], [o2]) where
--  fromDynSimFun f accum = (map unsafeFromDyn o1, map unsafeFromDyn o2)
--          -- use pattern (o1:o2:_) and not not [o1,o2]
--          -- because the second one doesn't when o1 and o2 are infinite lists
--          where (o1:o2:_) = f (reverse accum) 
--
-- instance (ProcType o1, ProcType o2) => 
--          SysFun2SimFun (Signal o1, Signal o2) (IO ([o1], [o2])) where
--  fromTHStrSimFun f accum = do
--         [o1, o2] <- f (reverse accum)
--         return (map parseProcType o1, map parseProcType o2) 
--
--
-- @
funOutInstances :: Int -- ^ number of outputs to generate
                -> Q (Dec, Dec, Dec)
funOutInstances n = do
 -- Generate N output names
 outNames <- replicateM n (newName "o")
 
 -- 1) Generate applyFun
 --    Generate an input tuple pattern for applyFun
 --    (o1, o2, ..., on)
 let tupPatApply = tupP (map varP outNames)
 --     Generate the output primitive signal list expression for applyFun
 --    [unsignal o1, unsignal o2, ...., unsignal on]
     outPrimSignalsApply = 
        listE $ map (\oName -> varE 'unSignal `appE` varE oName) outNames
 --    Generate the output signal types for ApplyFun
 --    [typeOf o1, typeOf o2, ...., typeOf on]
     outTypeRepsApply =
        listE $ map (\oName -> varE 'fsdTypeOf `appE` varE oName) outNames
 --    Generate the full output expression
     outEApply = [| ($outPrimSignalsApply, [], $outTypeRepsApply) |]
 --    Finally, the full declaration of applyFun 
     applySysFunDec = 
       funD 'applySysFun [clause [tupPatApply, wildP] (normalB outEApply) []]
 -- 2) Generate fromListSysFun
 --    Generate the parameter names
 fParFromSys <- newName "f"
 accumParFromSys <- newName "accum"
 --    Generate the parameter patterns
 let fPatFromSys = varP fParFromSys
     accumPatFromSys = varP accumParFromSys 
 --    Generate the list pattern: [o1, o2, .., on]
     listPatFromSys = listP $ map varP outNames
 --    Generate the rhs of the where declaration
     whereRHSFromSys = 
         [| $(varE fParFromSys) (reverse $(varE accumParFromSys)) |]
 --    Generate the where clause declaration
     whereDecFromSys = valD listPatFromSys (normalB whereRHSFromSys) []
 --    Generate output expression: (Signal o1, Signal o2, ..., Signal on)
     outEFromSys = 
          tupE $ map (\oName -> conE 'Signal `appE` varE oName) outNames
 --    Finally, the full declaration of fromListSysFun
     fromListSysFunDec = 
          funD 'fromListSysFun [clause [fPatFromSys, accumPatFromSys] 
                                       (normalB outEFromSys) 
                                       [whereDecFromSys]             ]   
 -- 3) Generate fromDynSimFun reusing parts of fromListSysFun
 --    Generate the list pattern: o1:o2: .. on:_
     listPatFromDynSim = foldr (\oName pat -> infixP (varP oName) '(:) pat) 
                               wildP outNames
 --    Generate the rhs of the where declaration
     whereRHSFromDynSim = 
         [| $(varE fParFromSys) (reverse $(varE accumParFromSys)) |]
 --    Generate the where clause declaration
     whereDecFromDynSim = valD listPatFromDynSim (normalB whereRHSFromSys) []
 --    Generate output expression: (Signal o1, Signal o2, ..., Signal on)
     outEFromDynSim = tupE $ map 
            (\oName -> varE 'map `appE` varE 'unsafeFromDyn `appE` varE oName)
            outNames
 --    Finally, the full declaration of fromDynSimFun
     fromDynSimFunDec = 
          funD 'fromDynSimFun [clause [fPatFromSys, accumPatFromSys] 
                                       (normalB outEFromDynSim) 
                                       [whereDecFromDynSim] ]   
 -- 4) Generate fromTHStrSimFun reusing parts of fromListSysFun
     listPatFromTHStrSim = listP $ map varP outNames
 --    Generate the rhs of the where declaration
     doFromTHStrSim = doE $
       [bindS listPatFromDynSim  [| $(varE fParFromSys) 
                                    (reverse $(varE accumParFromSys)) |],
        noBindS $ 
           (varE 'return) `appE`
           (tupE $ map 
            (\oName -> varE 'map `appE` varE 'parseProcType `appE` varE oName)
            outNames) ]
 --    Finally, the full declaration of fromThStrSimFun
     fromTHStrSimFunDec = 
          funD 'fromTHStrSimFun [clause [fPatFromSys, accumPatFromSys] 
                                       (normalB doFromTHStrSim) 
                                       []          ]   
 -- 5) Generate the SysFun instance
 --    We reuse the output signal names for the head type variables
 --    (Signal o1, Signal o2, ..., Signal on)
     signalTupT = if n == 1 then conT ''Signal `appT` varT (head outNames)
                            else foldl accumApp (tupleT n) outNames 
       where accumApp accumT vName =  
                       accumT `appT` (conT ''Signal `appT` varT vName)
 --    Create the ProcType context
     procTypeCxt = map (\vName -> appT (conT ''ProcType) (varT vName)) outNames

 --    Finally return the instance declaration
     sysFunIns = instanceD (cxt procTypeCxt) 
                           (conT ''SysFun `appT` signalTupT) 
                           [applySysFunDec, fromListSysFunDec]
 -- 6) Generate the SysFun2SimFun instance 
     listTupT = if n == 1 then listT `appT` varT (head outNames)
                          else foldl accumApp (tupleT n) outNames 
       where accumApp accumT vName  =  
                       accumT `appT` (listT `appT` varT vName)
     simFunIns = instanceD (cxt procTypeCxt) 
                    (conT ''SysFunToSimFun `appT` signalTupT `appT` listTupT) 
                    [fromDynSimFunDec]
 -- 7) Generate the SysFun2IOSimFun instance
     ioSimFunIns = instanceD (cxt procTypeCxt)
                (conT ''SysFunToIOSimFun `appT` 
                      signalTupT `appT` 
                      (conT ''IO `appT` listTupT))
                [fromTHStrSimFunDec]
 -- Finally, return the declarations
 liftM3 (,,) sysFunIns simFunIns ioSimFunIns

---------------------------------------------------------------
-- Checking the type of a System Function with Template Haskell
---------------------------------------------------------------

-- | Given a function type expressed with Template Haskell, check
--   whether it complies with the expected type of a system function
--   and, in that case, provide the type and number of the system
--   inputs and outputs.
checkSysFType :: Type -> Q (([Type],Int),([Type],Int))
checkSysFType t = do let (inputTypes, retType, context) = unArrowT t
                         (outCons, outTypes,  _)        = unAppT retType     
                     -- Discard polymorphic system functions
                     -- FIXME: We could support polymorphic signals, but it
                     -- requires more work and we don't still know if it 
                     -- will be necessary anyway.
                     when (isPoly context) (qGiveUp name)
                     -- Check that all inputs are signals
                     when (not $ all isSignalT inputTypes) (qGiveUp name)
                     -- Check the outputs
                     outInfo <- checkSysFOutputs (outCons, outTypes)
                     return ((inputTypes, length inputTypes), outInfo)
 where name = "ForSyDe.System.checkSysFType"


-- | Check the output types of the system function given its
--   constructor and its type arguments
checkSysFOutputs :: (Type, [Type]) -> Q ([Type], Int)
-- The system function doesn't output any signals at all
checkSysFOutputs (ConT n, []) |  n == ''() = return ([],0)
-- The system function just returns a signal
checkSysFOutputs (ConT s, [arg]) | s == ''Signal  = 
 return ([ConT ''Signal  `AppT` arg ], 1)
-- The system function returns various signals in a tuple
-- NOTE: Unfortunatelly due to ghc's bug 1849 TupleT is never returned by reify
-- so we have to match the constructor with a regex
-- FIXME: Update this function whenever the bug is fixed 
--        http://hackage.haskell.org/trac/ghc/ticket/1849
checkSysFOutputs (ConT name, outTypes) 
 | show name =~ "^Data\\.Tuple\\.\\(,+\\)$" =  do
     when (not $ all isSignalT outTypes) (qGiveUp checkSysFOutputsNm)
     return (outTypes, length outTypes)
-- Otherwise the function output type is incorrect
checkSysFOutputs _ = qGiveUp checkSysFOutputsNm

checkSysFOutputsNm :: String
checkSysFOutputsNm = "ForSyDe.System.SysFun.checkSysFOutputs"

-- | Check if a type corresponds to a signal
isSignalT :: Type -> Bool
isSignalT ((ConT name)  `AppT` _ ) | name == ''Signal = True
isSignalT _                                           = False

-- | Unsafely transform from a dynamic value
unsafeFromDyn :: forall a . Typeable a => Dynamic -> a
unsafeFromDyn dyn = fromDyn dyn err 
  where err = intError "unsafeFromDyn" (DynMisMatch dyn targetType) 
        targetType = typeOf (undefined :: a)

-- | Parse a proctype value
parseProcType :: forall a .ProcType a => String -> a
parseProcType s = 
    case (readP_to_S readProcType) s of 
        [(a,_)] -> a
        _ -> error ("parseProcType: error parsing element of type " ++
                    show (typeOf (undefined :: a)) )       
