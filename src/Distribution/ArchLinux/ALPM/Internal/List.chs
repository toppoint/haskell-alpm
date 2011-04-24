{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="alpm" prefix="alpm" #}

module Distribution.ArchLinux.ALPM.Internal.List where

#include <alpm_list.h>

{# import Distribution.ArchLinux.ALPM.Internal.Types #}

import Control.Monad

import Foreign
import Foreign.C

newtype List a = List (Ptr (List a))

unList :: List a -> Ptr (List a)
unList (List ptr) = ptr 

{- TODO
 - Bind the free callback function with some allocation handling.
-}

-- Generic Function Bindings --------------------------------------------------

type ListCmp a b = a -> b -> Int 
foreign import ccall "wrapper" 
  mkListCmp :: (Ptr a -> Ptr b -> CInt) 
                 -> IO (FunPtr (Ptr a -> Ptr b -> CInt))

alpmListSortWrapper :: (ALPMType a,ALPMType b) => ListCmp a b -> Ptr a -> Ptr b -> CInt
alpmListSortWrapper f c1 c2 =
  fromIntegral $ f (pack c1) (pack c2)

-- List Mutaters --------------------------------------------------------------

addList :: ALPMType a =>  List a -> a -> IO (List a)
addList list ptr = 
  liftM List $ 
    alpm_list_add (unList list) (unpack ptr)

joinList :: List a -> List a -> IO (List a)
joinList lst1 lst2 =
  liftM List $
    alpm_list_join (unList lst1) (unList lst2)

addSortedList :: ALPMType a => List a -> a -> ListCmp a a -> IO (List a)
addSortedList list dat sort = do
  csort <- mkListCmp (alpmListSortWrapper sort)
  newList <- liftM List $ alpm_list_add_sorted (unList list) (unpack dat) csort 
  freeHaskellFunPtr csort
  return newList 

mergeList :: ALPMType a => List a -> List a -> ListCmp a a -> IO (List a)
mergeList lst1 lst2 sort = do
  csort <- mkListCmp (alpmListSortWrapper sort)
  newList <- liftM List $ alpm_list_mmerge (unList lst1) (unList lst2) csort
  freeHaskellFunPtr csort
  return newList 

-- Item Mutators -------------------------------------------------------------

msortList :: ALPMType a => List a -> Int -> ListCmp a a -> IO (List a)
msortList list n sort = do
  csort <- mkListCmp (alpmListSortWrapper sort)
  newList <- liftM List $ alpm_list_msort (unList list) (fromIntegral n) csort
  freeHaskellFunPtr csort
  return newList 

removeItemList :: List a -> List a -> IO (List a)
removeItemList lst1 lst2 = do
  liftM List $ alpm_list_remove_item (unList lst1) (unList lst2)

removeList :: (ALPMType a,ALPMType b) => List a -> b -> ListCmp a b -> IO (a,List a)
removeList lst needle sort = do
  csort <- mkListCmp (alpmListSortWrapper sort)
  alloca $ \dataPtr -> do 
    newList <- liftM List $ alpm_list_remove (unList lst) (unpack needle) csort dataPtr
    freeHaskellFunPtr csort
    ptr <- liftM castPtr $ peek dataPtr
    return (pack ptr,newList)

{- TODO
alpm_list_t *alpm_list_remove_str(alpm_list_t *haystack, const char *needle, char **data);
-}

removeDupesList :: List a -> IO (List a)
removeDupesList = liftM List . alpm_list_remove_dupes . unList

strdupList :: List a -> IO (List a)
strdupList = liftM List . alpm_list_strdup . unList

copyList :: List a -> IO (List a)
copyList =  liftM List . alpm_list_copy . unList

-- TODO - Do we really need this function, for this the type parameter of List
-- should be an instance of Storable   
-- alpm_list_t *alpm_list_copy_data(const alpm_list_t *list, size_t size);

reverseList :: List a -> IO (List a)
reverseList = liftM List . alpm_list_reverse . unList


-- Item Accessors -------------------------------------------------------------

firstList :: List a -> IO (List a)
firstList = liftM List . alpm_list_first . unList

nthList :: List a -> Int -> IO (List a)
nthList lst n = liftM List $
  alpm_list_nth (unList lst) (fromIntegral n)

nextList :: List a -> IO (List a)
nextList = liftM List . alpm_list_next . unList

lastList :: List a -> IO (List a)
lastList = liftM List . alpm_list_last . unList

getDataList :: ALPMType a => List a -> IO (Maybe a)
getDataList list = do
  element <- alpm_list_getdata $ unList list
  if element == nullPtr 
    then return Nothing
    else return . Just $ pack element

fromList :: ALPMType a => List a -> IO [a]
fromList list = do
  -- rewind list
  flist <- firstList list
  -- extract elements
  whileJust ( \lst -> do
    el <- getDataList lst
    lst' <- nextList lst
    return (el,lst')
    ) flist
 where whileJust :: (b -> IO (Maybe a,b)) -> b -> IO [a]
       whileJust f isNull = do
        (mEntry, isNull') <- f isNull 
        case mEntry of
          Just x  -> do 
            lst <- whileJust f isNull'
            return $ x : lst
          Nothing -> return []

toList :: ALPMType a => [a] -> IO (List a)
toList lst = do
  list <- foldM addList (List $ castPtr nullPtr) lst
  return list

-- Misc -----------------------------------------------------------------------

countList :: List a -> IO Int
countList = liftM fromIntegral . alpm_list_count . unList

findList :: (ALPMType a,ALPMType b) => List a -> b -> ListCmp a b -> IO (Maybe a)
findList list needle sort = do
  csort <- mkListCmp (alpmListSortWrapper sort)
  needleData <- alpm_list_find (unList list) (unpack needle) csort
  if needleData == nullPtr 
    then return Nothing
    else return .Just $ pack needleData

{- TODO
void *alpm_list_find_ptr(const alpm_list_t *haystack, const void *needle);
char *alpm_list_find_str(const alpm_list_t *haystack, const char *needle);
alpm_list_t *alpm_list_diff(const alpm_list_t *lhs, const alpm_list_t *rhs, alpm_list_fn_cmp fn);
void alpm_list_diff_sorted(const alpm_list_t *left, const alpm_list_t *right,
		alpm_list_fn_cmp fn, alpm_list_t **onlyleft, alpm_list_t **onlyright);
-}

foreign import ccall safe "alpm_list.h alpm_list_add"
  alpm_list_add :: Ptr (List a) -> Ptr a -> IO (Ptr (List a))

foreign import ccall safe "alpm_list.h alpm_list_join"
  alpm_list_join :: Ptr (List a) -> Ptr (List a) -> IO (Ptr (List a))

foreign import ccall safe "alpm_list.h alpm_list_add_sorted"
  alpm_list_add_sorted :: Ptr (List a) -> Ptr a -> FunPtr (Ptr a -> Ptr a -> CInt) -> IO (Ptr (List a))

foreign import ccall safe "alpm_list.h alpm_list_mmerge"
  alpm_list_mmerge :: Ptr (List a) -> Ptr (List a) -> FunPtr (Ptr a -> Ptr a -> CInt) -> IO (Ptr (List a))

foreign import ccall safe "alpm_list.h alpm_list_remove_dupes"
  alpm_list_remove_dupes :: Ptr (List a) -> IO (Ptr (List a))

foreign import ccall safe "alpm_list.h alpm_list_strdup"
  alpm_list_strdup :: Ptr (List a) -> IO (Ptr (List a))

foreign import ccall safe "alpm_list.h alpm_list_copy" 
  alpm_list_copy :: Ptr (List a) -> IO (Ptr (List a))

foreign import ccall safe "alpm_list.h alpm_list_reverse" 
  alpm_list_reverse :: Ptr (List a) -> IO (Ptr (List a))

foreign import ccall safe "alpm_list.h alpm_list_first" 
  alpm_list_first :: Ptr (List a) -> IO (Ptr (List a))

foreign import ccall safe "alpm_list.h alpm_list_next" 
  alpm_list_next :: Ptr (List a) -> IO (Ptr (List a))

foreign import ccall safe "alpm_list.h alpm_list_last" 
  alpm_list_last :: Ptr (List a) -> IO (Ptr (List a))

foreign import ccall safe "alpm_list.h alpm_list_nth" 
  alpm_list_nth :: Ptr (List a) -> CInt -> IO (Ptr (List a))

foreign import ccall safe "alpm_list.h alpm_list_getdata" 
  alpm_list_getdata :: Ptr (List a) -> IO (Ptr a)

foreign import ccall safe "alpm_list.h alpm_list_count" 
  alpm_list_count :: Ptr (List a) -> IO  CInt 

foreign import ccall safe "alpm_list.h alpm_list_msort" 
  alpm_list_msort :: Ptr (List a) -> CInt -> FunPtr (Ptr a -> Ptr a -> CInt) -> IO (Ptr (List a))

foreign import ccall safe "alpm_list.h alpm_list_remove_item" 
  alpm_list_remove_item :: Ptr (List a) -> Ptr (List a) -> IO (Ptr (List a))

foreign import ccall safe "alpm_list.h alpm_list_remove"
  alpm_list_remove :: Ptr (List a) -> Ptr b -> FunPtr (Ptr a -> Ptr b -> CInt) -> Ptr (Ptr a) -> IO (Ptr (List a))

foreign import ccall safe "alpm_list.h alpm_list_find"
  alpm_list_find :: Ptr (List a) -> Ptr b -> FunPtr (Ptr a -> Ptr b -> CInt) -> IO (Ptr a)
