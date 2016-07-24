{-# LANGUAGE TemplateHaskell #-}

module ForSyDe.Deep.Backend.VHDL.Translate.HigherOrderFunctions (
        isHigherOrderFunction,
        translateHigherOrderFunctionBody
        ) where

import Language.Haskell.TH
import qualified Language.Haskell.TH as TH
import ForSyDe.Deep.Backend.VHDL.AST
import qualified ForSyDe.Deep.Backend.VHDL.AST as VHDL
import qualified Data.Param.FSVec as V

import ForSyDe.Deep.Backend.VHDL.Traverse.VHDLM
import Data.Maybe (isJust)
import Data.Data (tyconUQname)


isHigherOrderFunction :: TH.Exp -> Bool
isHigherOrderFunction = isJust.getHoF

translateHigherOrderFunctionBody :: TH.Exp -> VHDLM [SeqSm]
translateHigherOrderFunctionBody exp = case getHoF exp of
                                        (Just transF) -> transF exp
                                        -- This should not happen (translation should be guarded by isHigherOrderFunction)
                                        Nothing        -> error $ "This is not a supported higher-order function: "++(show.ppr $ exp)

type TrFun = TH.Exp -> VHDLM [VHDL.SeqSm]
-- -------------------------------------
-- Name Table for higher order functions
-- -------------------------------------

getHoF :: TH.Exp -> Maybe TrFun
getHoF exp@(AppE _ _) = lookup (fname, nargs) hofNameTable
  where
    ((VarE fname), _, nargs) = unApp exp
getHoF _ = Nothing



hofNameTable :: [((TH.Name, Arity), TrFun)]
hofNameTable = [(('V.map,       2), translateMap),
                (('V.foldl,     3), translateFold "RANGE"),
                (('V.foldr,     3), translateFold "REVERSE_RANGE")]


-- ---------------------
-- Translation functions
-- ---------------------

translateMap :: TH.Exp -> VHDLM [VHDL.SeqSm]
translateMap (((VarE mapname) `AppE` (VarE fname)) `AppE` (VarE _)) | mapname == 'V.map = do
  fnameV   <- transTHName2VHDL fname
  loopvar  <- genVHDLId "i"
  retnameV <- genVHDLId "ret"

  fSpec <- getCurrentFunctionSpec
  let Function _ [IfaceVarDec argnameV _] rettype = fSpec

  -- variable ret : rettype
  let vardecl = SPVD $ VarDec retnameV (SubtypeIn rettype Nothing) Nothing
  addDecsToFunTransST [vardecl]

  -- for i in arg'range loop
  --    ret(i) := f(arg(i))
  -- end loop
  -- return ret
  let rangeAttrib = unsafeVHDLBasicId "RANGE"
      range       = AttribRange $ AttribName (NSimple argnameV) rangeAttrib Nothing
      body        = [(retnameV!:loopvar) := (fnameV $: [argnameV!:loopvar])]
  return $ [ForSM loopvar range body, ReturnSm . Just . PrimName . NSimple $ retnameV]
translateMap e = error $ "Invalid application of map: got `"++(pprint e)++"` expected `map f arg`"


translateFold :: String -> TH.Exp -> VHDLM [VHDL.SeqSm]
translateFold attributeName ((((VarE foldlname) `AppE` (VarE fname)) `AppE` (VarE _)) `AppE` (VarE _)) | foldlname == 'V.foldl = do
  fnameId   <- transTHName2VHDL fname
  loopvarId  <- genVHDLId "i"
  stateId <- genVHDLId "state"

  --- Nat s => (a -> b -> a) -> a -> FSVec s b -> a 
  fSpec <- getCurrentFunctionSpec
  let Function _ [IfaceVarDec initId stateType,
                  IfaceVarDec argId _] _ = fSpec
  -- assert that stateType == rettype?

  -- variable tmp : stateType
  let vardecl = SPVD $ VarDec stateId (SubtypeIn stateType Nothing) Nothing
  addDecsToFunTransST [vardecl]

  -- state := init
  let initStmt = (NSimple stateId) := (PrimName . NSimple $ initId)

  -- for i in arg'range loop
  --    state := f(state, arg(i))
  -- end loop
  let rangeAttrib = unsafeVHDLBasicId attributeName
      range       = AttribRange $ AttribName (NSimple argId) rangeAttrib Nothing
      body        = [(NSimple stateId) := (fnameId $: [NSimple stateId, argId!:loopvarId])]
      forLoopStmt = ForSM loopvarId range body

  -- return state
  let retStmt = ReturnSm . Just . PrimName . NSimple $ stateId

  return [initStmt, forLoopStmt, retStmt]
translateFold _ e = error $ "Invalid application of foldl: got `"++(pprint e)++"` expected `foldl f arg`"


-- ---------
-- Utilities
-- ---------

-- AST level indexing operator
(!:) :: VHDLId -> VHDLId -> VHDLName
(!:) name index = NIndexed $ IndexedName (NSimple name) [PrimName $ NSimple index]

-- AST level function call operator
($:) :: VHDLId -> [VHDLName] -> VHDL.Expr
($:) fname args = PrimFCall $ FCall (NSimple fname) (map (\a -> Nothing :=>: (ADName a)) args)

-- AST level assignment operator
-- (:=) :: VHDLName -> VHDL.Expr -> VHDL.SeqSm
-- defined in ForSyDe.Deep.Backend.VHDL.AST

-- AST level association operator
-- (:=>: :: Maybe SimpleName -> ActualDesig -> AssocElem
-- defined in ForSyDe.Deep.Backend.VHDL.AST

-- unApply an expression and obtain the number of arguments found
unApp :: Exp -> (Exp, [Exp], Int)
unApp e = (first, rest, n)
 where (first:rest, n) = unAppAc ([],0) e
       unAppAc (xs,n) (f `AppE` arg) = unAppAc (arg:xs, n+1) f
       unAppAc (xs,n) f = (f:xs,n)

transTHName2VHDL :: TH.Name -> VHDLM VHDLId
transTHName2VHDL = liftEProne . mkVHDLExtId . tyconUQname  . pprint

