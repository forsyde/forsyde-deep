{-# LANGUAGE ScopedTypeVariables #-} 
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Netlist.Traverse
-- Copyright   :  (c) ES Group, KTH/ICT/ES 2007-2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (LSTV)
--
-- This module provides traversing operations for 'ForSyDe.Deep.Netlist'
--
--
-- /This module is based on Lava2000/: <http://www.cs.chalmers.se/~koen/Lava/>
-- 
-----------------------------------------------------------------------------
module ForSyDe.Deep.Netlist.Traverse  where


import ForSyDe.Deep.Netlist
import ForSyDe.Deep.OSharing
import ForSyDe.Deep.ForSyDeErr


import Data.Maybe (fromMaybe)
-- qualified to avoid nameclash
import qualified Data.Foldable  as DF (Foldable(foldMap), toList)
import Data.Monoid (mempty)
-- qualified to avoid nameclash
import qualified Data.Traversable as DT (Traversable(traverse,mapM)) 
import Control.Applicative (pure, (<$>))
import Control.Monad.State
import Control.Monad.ST (ST)

-- Instances to traverse a netlist Node (and implicitly the whole netlist)

instance DF.Foldable NlProc where
 foldMap _ (Const _)  = mempty 
 foldMap f (ZipWithNSY _ is)   = DF.foldMap f is
 foldMap f (ZipWithxSY _ is) = DF.foldMap f is
 foldMap f (UnzipNSY _ _ i)    = f i
 foldMap f (UnzipxSY _ _ _ i)  = f i                                  
 foldMap f (DelaySY _ i)       = f i
 foldMap f (SysIns  _ i)       = DF.foldMap f i



instance DF.Foldable NlNode where
 foldMap _ (InPort  _)   = mempty
 foldMap f (Proc _ proc) = DF.foldMap f proc

instance Functor NlProc where
 fmap _ (Const val)          = Const val
 fmap f (ZipWithNSY pf is)  = ZipWithNSY pf (fmap f is)
 fmap f (ZipWithxSY pf is)  = ZipWithxSY  pf (fmap f is)
 fmap f (UnzipNSY ts pf i)  = UnzipNSY ts pf (f i)
 fmap f (UnzipxSY t n pf i) = UnzipxSY t n pf (f i)                       
 fmap f (DelaySY c i)       = DelaySY c (f i)
 fmap f (SysIns def is)     = SysIns def (fmap f is)



instance Functor NlNode where
 fmap _ (InPort  id)         = InPort  id
 fmap f (Proc id proc)       = Proc id (fmap f proc) 

instance DT.Traversable NlProc where
 traverse _ (Const val) = pure (Const val) 
 traverse f (ZipWithNSY pf is)  = ZipWithNSY pf <$> DT.traverse f is
 traverse f (ZipWithxSY pf is)  = ZipWithxSY pf <$> DT.traverse f is
 traverse f (UnzipNSY ts pf i)  = UnzipNSY ts pf <$> f i
 traverse f (UnzipxSY t n pf i) = UnzipxSY t n pf <$> f i
 traverse f (DelaySY c i)       = DelaySY c <$> f i
 traverse f (SysIns def is)     = SysIns def <$> DT.traverse f is




instance DT.Traversable NlNode where
 traverse _ (InPort  id)   = pure (InPort id)
 traverse f (Proc id proc) = Proc id <$> DT.traverse f proc 


-- | Traversing monad, stacking state and error transformers over IO
type TravSEIO s e a = (StateT s (ErrorT e IO)) a



-- | traverseSIO traverses a netlist and returns a final user-defined 
--   traversing state (@s@) given:
--  new: generates a new (and normally unique) tag for the outputs of each 
--       netlist node given the traversing state (which is possibly updated 
--       as well).
--  define: given the output tags of a node, current iteration state, 
--          and the output tags of its children, @define@
--          generates the netlist of that node, possibly updating 
--          the traversing state
-- FIXME: shoudn't the arguments of define go the other way around?
--        first inputs then outputs.
-- FIXME: why are tags needed in define? [oinfo] should be enough.
--        tags are ugly in general (see the pattern matches) fix this problem.
traverseSEIO :: (DT.Traversable container, Error e) => 
         (NlNode NlSignal -> TravSEIO s e [(NlNodeOut, oinfo)]) -- ^ new
      -> ([(NlNodeOut, oinfo)] -> NlNode oinfo -> TravSEIO s e ()) -- ^ define
      -> Netlist container 
      -> TravSEIO s e (container oinfo)
traverseSEIO new define (Netlist rootSignals) =
  do uRefTable <- liftIO $ newURefTableIO

     let gather (NlTree (NlEdge nodeRef tag)) =
           do visited <- liftIO $ queryIO uRefTable nodeRef
              case visited of
                Just infoPairs  -> return (specifyOut tag infoPairs)
                Nothing -> do 
                  let node = readURef nodeRef
                  infoPairs <- new node
                  liftIO $ addEntryIO uRefTable nodeRef infoPairs
                  childInfo <- DT.mapM gather node
                  define infoPairs childInfo
                  return (specifyOut tag infoPairs)

         specifyOut :: NlNodeOut -> [(NlNodeOut, a)] -> a
         specifyOut tag pairs = fromMaybe err maybeOut
             where funName = "ForSyDe.NetList.Traverse.traverseIO"
                   err = intError funName InconsOutTag
                   maybeOut = lookup tag pairs

     DT.mapM gather rootSignals

-- | Traversing monad, stacking state and error transformers over ST
type TravSEST s e st a = (StateT s (ErrorT e (ST st))) a

-- | 'ST'-monad  version of 'traverseSEIO'
traverseSEST :: (DT.Traversable container, Error e) => 
         (NlNode NlSignal -> TravSEST s e st [(NlNodeOut, oinfo)]) -- ^ new
      -> ([(NlNodeOut, oinfo)] -> NlNode oinfo -> TravSEST s e st ()) -- ^ define
      -> Netlist container 
      -> TravSEST s e st (container oinfo)
traverseSEST new define (Netlist rootSignals) =
  do let lift2 = lift.lift
     uRefTable <- lift2 $ newURefTableST
     let gather (NlTree (NlEdge nodeRef tag)) =
           do visited <- lift2 $ queryST uRefTable nodeRef
              case visited of
                Just infoPairs  -> return (specifyOut tag infoPairs)
                Nothing -> do 
                  let node = readURef nodeRef
                  infoPairs <- new node
                  lift2 $ addEntryST uRefTable nodeRef infoPairs
                  childInfo <- DT.mapM gather node
                  define infoPairs childInfo
                  return (specifyOut tag infoPairs)

         specifyOut :: NlNodeOut -> [(NlNodeOut, a)] -> a
         specifyOut tag pairs = fromMaybe err maybeOut
             where funName = "ForSyDe.NetList.Traverse.traverseIO"
                   err = intError funName InconsOutTag
                   maybeOut = lookup tag pairs

     DT.mapM gather rootSignals

-- deprecated, do not use
traverseST :: DT.Traversable container => 
             (NlNode NlSignal -> ST s [(NlNodeOut, oinfo)]) 
          -> ([(NlNodeOut, oinfo)] -> NlNode oinfo -> ST s ()) 
          -> Netlist container 
          -> ST s (container oinfo)
traverseST new define (Netlist rootSignals) =
  do uRefTable <- newURefTableST

     let gather (NlTree (NlEdge nodeRef tag)) =
           do visited <- queryST uRefTable nodeRef
              case visited of
                Just infoPairs  -> return (specifyOut tag infoPairs)
                Nothing -> do let node = readURef nodeRef
                              infoPairs <- new node
                              addEntryST uRefTable nodeRef infoPairs
                              childInfo <- DT.mapM gather node
                              define infoPairs childInfo
                              return (specifyOut tag infoPairs)

         specifyOut :: NlNodeOut -> [(NlNodeOut, a)] -> a
         specifyOut tag pairs = fromMaybe err maybeOut
             where funName = "ForSyDe.NetList.Traverse.traverseST"
                   err = intError funName InconsOutTag
                   maybeOut = lookup tag pairs

     DT.mapM gather rootSignals

-- | Obtain the arguments of a node
arguments :: NlNode a -> [a]
arguments = DF.toList
