module Data.Typeable.FSDTypeRepLib (
        FSDType,
        FSDTypeRep,
        FSDTypeCon,
        fsdTy,
        fsdTyCon,
        fsdTyConApp,
        fsdTupleTyCon,
        fsdTyConName,
        fsdUnArrowT,
        fsdTyRep,
        fsdTyConOf,
        fsdTypeOf,
        fsdSplitTyConApp,
        type2FSDTypeRep
) where

import Data.Typeable
import Data.Typeable.TypeRepLib
import Data.Typeable.Internal
import Language.Haskell.TH (Type)
import Language.Haskell.TH.TypeLib (type2TypeRep)

newtype FSDTypeRep = FSDTypeRep' TypeRep deriving (Eq, Ord, Show)
newtype FSDTypeCon = FSDTypeCon' TyCon   deriving (Eq, Ord, Show)

class FSDType a where
        fsdTy :: a -> FSDTypeRep

-- | e.g. use as fsd.typeOf (undefined :: FSVec)
instance FSDType TypeRep where
        fsdTy tr = FSDTypeRep' $ typeRepNormalize tr

--instance FSDType Type where
--        fsdTy ty = FSDTypeRep' typeRepNormalize $ type2TypeRep ty


fsdTyRep :: FSDTypeRep -> TypeRep
fsdTyRep (FSDTypeRep' tr) = tr

fsdTyCon :: FSDTypeRep -> FSDTypeCon
fsdTyCon (FSDTypeRep' tr) = FSDTypeCon' $ typeRepTyCon tr

fsdUnTyCon :: FSDTypeCon -> TyCon
fsdUnTyCon (FSDTypeCon' tc) = tc

fsdSplitTyConApp :: FSDTypeRep -> (FSDTypeCon, [FSDTypeRep])
fsdSplitTyConApp (FSDTypeRep' fsdtr) = (FSDTypeCon' tc, map FSDTypeRep' tr) where
                                        (tc, tr) = splitTyConApp fsdtr

fsdTyConOf :: (Typeable a) => a -> FSDTypeCon
fsdTyConOf = fsdTyCon.fsdTy.typeOf

fsdTypeOf :: (Typeable a) => a -> FSDTypeRep
fsdTypeOf = fsdTy.typeOf

fsdTyConApp :: FSDTypeCon -> [FSDTypeRep] -> FSDTypeRep
fsdTyConApp c trs = FSDTypeRep' $ mkTyConApp tc targs where
                tc    = fsdUnTyCon c
                targs = map fsdTyRep trs

fsdTupleTyCon :: Int -> FSDTypeCon
fsdTupleTyCon nOuts = FSDTypeCon' $ mkTyCon3 "" "GHC.Tuple" $ '(':replicate (nOuts-1) ','++")"

fsdTyConName (FSDTypeCon' tc) = tyConName tc

fsdUnArrowT :: FSDTypeRep        -- ^ TypeRep to observe  
        ->  ([FSDTypeRep], FSDTypeRep) -- ^ (args 'TypeRep', ret 'TypeRep')
fsdUnArrowT rep
 | repCon == arrowTyCon = let (args', ret') = fsdUnArrowT  arrowArg2
                          in (arrowArg1:args', ret')
 | otherwise = ([], rep)
 where (repCon,~[arrowArg1,arrowArg2]) = fsdSplitTyConApp rep

arrowTyCon :: FSDTypeCon
arrowTyCon = fsdTyConOf (undefined :: () -> ())

type2FSDTypeRep :: Type -> Maybe FSDTypeRep
type2FSDTypeRep ftr = do
        tr <- type2TypeRep ftr
        Just $ fsdTy tr

--
-- Strip the package names out of the TypeRep for correct comparison
--
tyConNormalize :: TyCon -> TyCon
tyConNormalize tc = mkTyCon3 "" (tyConModule tc) (tyConName tc)

typeRepNormalize :: TypeRep -> TypeRep
typeRepNormalize tr = mkPolyTyConApp tcN kindsN argsN
 where
        tc      = typeRepTyCon tr
        kinds   = typeRepKinds tr
        args    = typeRepArgs  tr
        tcN     = tyConNormalize tc
        kindsN  = map typeRepNormalize kinds
        argsN   = map typeRepNormalize args
