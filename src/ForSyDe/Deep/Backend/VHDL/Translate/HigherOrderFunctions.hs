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
import Control.Monad (when)
import Control.Monad.State


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
hofNameTable = [(('V.foldl,     3), translateFold    'V.foldl "RANGE"),
                (('V.foldr,     3), translateFold    'V.foldr "REVERSE_RANGE"),
                (('V.map,       2), translateZipWith 'V.map 2),
                (('V.zipWith,   3), translateZipWith 'V.zipWith 3),
                (('V.zipWith3,  4), translateZipWith 'V.zipWith3 4)]


-- ---------------------
-- Translation functions
-- ---------------------

translateFold :: TH.Name -> String -> TH.Exp -> VHDLM [VHDL.SeqSm]
translateFold selfName attributeName exp@((((VarE foldname) `AppE` (VarE fname)) `AppE` (VarE _)) `AppE` (VarE _)) | foldname == selfName = do
  fnameId   <- transTHName2VHDL fname
  loopvarId  <- genVHDLId "i"
  stateId <- genVHDLId "state"

  --- Nat s => (a -> b -> a) -> a -> FSVec s b -> a 
  fSpec <- getCurrentFunctionSpec
  let Function _ [IfaceVarDec initId stateType,
                  IfaceVarDec argId _] _ = fSpec

  assertNormalForm exp fSpec

  -- check whether this is a known operator
  knownTranslation <- lookupName fname
  let fCall = case knownTranslation of 
            Just (arity, genFCallExp) -> (\args -> genFCallExp (map PrimName args))
            Nothing                   -> (\args -> fnameId $: args)

  -- variable state : stateType
  let vardecl = SPVD $ VarDec stateId (SubtypeIn stateType Nothing) Nothing
  addDecsToFunTransST [vardecl]

  -- state := init
  let initStmt = (NSimple stateId) := (PrimName . NSimple $ initId)

  -- for i in arg'range loop
  --    state := f(state, arg(i))
  -- end loop
  let rangeAttrib = unsafeVHDLBasicId attributeName
      range       = AttribRange $ AttribName (NSimple argId) rangeAttrib Nothing
      body        = [(NSimple stateId) := (fCall [NSimple stateId, argId!:loopvarId])]
      forLoopStmt = ForSM loopvarId range body

  -- return state
  let retStmt = ReturnSm . Just . PrimName . NSimple $ stateId

  return [initStmt, forLoopStmt, retStmt]
translateFold selfName _ e = error $ "Invalid application of "++(show selfName)++": got `"++(pprint e)++"` expected `"++(show selfName)++" f arg`"


translateZipWith :: TH.Name -> Int -> TH.Exp -> VHDLM [VHDL.SeqSm]
translateZipWith selfName selfArity exp | (hofName == selfName) && (nArgs == selfArity) = do
    fnameV   <- transTHName2VHDL fname
    loopvar  <- genVHDLId "i"
    retnameV <- genVHDLId "ret"

    fSpec <- getCurrentFunctionSpec
    let Function _ _ rettype = fSpec

    assertNormalForm exp fSpec

    -- check whether this is a known operator
    knownTranslation <- lookupName fname
    let fCall = case knownTranslation of 
                  Just (arity, genFCallExp) -> (\args -> genFCallExp (map PrimName args))
                  Nothing                   -> (\args -> fnameV $: args)

    argNames <- mapM (\(VarE argName) -> transTHName2VHDL argName) thArgs

    -- variable ret : rettype
    let vardecl = SPVD $ VarDec retnameV (SubtypeIn rettype Nothing) Nothing
    addDecsToFunTransST [vardecl]

    -- for i in arg'range loop
    --    ret(i) := f(arg1(i), arg2(i), [...])
    -- end loop
    let rangeAttrib   = unsafeVHDLBasicId "RANGE"
        range         = AttribRange $ AttribName (NSimple retnameV) rangeAttrib Nothing
        indexedArgLst = map (\name -> name!:loopvar) argNames
        body          = [(retnameV!:loopvar) := (fCall indexedArgLst)]
        loopStmt      = ForSM loopvar range body
    -- return ret
    let returnStmt    = ReturnSm . Just . PrimName . NSimple $ retnameV

    return $ [loopStmt, returnStmt]
  where
    ((VarE hofName), (VarE fname):thArgs, nArgs) = unApp exp
translateZipWith selfName selfArity e = error $ "Invalid application of "++self++": got `"++exp++"` expected `"++self++" f "++args++"`"
        where self = show selfName
              exp  = pprint e
              args = unwords $ replicate selfArity "arg"


-- ---------
-- Utilities
-- ---------

-- AST level indexing operator
(!:) :: VHDLId -> VHDLId -> VHDLName
(!:) name index = NIndexed $ IndexedName (NSimple name) [PrimName $ NSimple index]

-- AST level function call operator
($:) :: VHDLId -> [VHDLName] -> VHDL.Expr
($:) fname args = PrimFCall $ FCall (NSimple fname) (map (\a -> Nothing :=>: (ADName a)) args)

-- AST level sequential assignment operator
-- (:=) :: VHDLName -> VHDL.Expr -> VHDL.SeqSm
-- defined in ForSyDe.Deep.Backend.VHDL.AST

-- AST level map-association operator
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

lookupName :: Name -> VHDLM (Maybe (Arity, [VHDL.Expr] -> VHDL.Expr))
lookupName name = do
  nameTable <- gets (nameTable.funTransST.local)
  return $ lookup name nameTable


-- Check whether the given function application and the given function
-- signature are in normal form. Raises an error when the normal form is
-- violated, otherwise simply returns ().
--
-- The signature is derived from the outer definition, while the application
-- expression represents the body of the specialized function 
-- Some specializations in normal form:
--   map_f v = map f v
--   foldl_f i v = foldl f i v
--   zipWith_f a b = zipWith f a b
--
-- Normal form means, that apart from the higher order argument, the
-- argument lists agree. This means that all arguments are explicitly written
-- and the names of all arguments are the same. This ensures, that the types
-- within the signature can be relied upon for translation purposes.
-- Additionally, the application of the specialized function needs to be the
-- sole expresssion in the function body, which ensures that the return type of
-- the function is valid as well.
--
-- Some specializations not in normal form:
--   map_f = map f
--    => incomplete argument list
--   foldl_f v i = foldl f i v
--    => order of arguments does not match
--   zipWith_f v = zipWith f v v
--    => duplicated argument
--
assertNormalForm :: TH.Exp -> VHDL.SubProgSpec -> VHDLM ()
assertNormalForm appl sig = do
  let (_, _:args, _) = unApp appl
      (Function _ argDecls _) = sig
  
  argNamesBody <- mapM (\(VarE name) -> transTHName2VHDL name) args
  let argNamesSig = map (\(IfaceVarDec name _) -> name) argDecls
  
  when (argNamesBody /= argNamesSig) $ error ("Higher order function application is not in normal form: "++(pprint appl)++"\n The argument list must agree")
  return ()
