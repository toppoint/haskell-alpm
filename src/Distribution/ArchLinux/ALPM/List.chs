{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="alpm" prefix="alpm" #}


module Distribution.ArchLinux.ALPM.List where

#include <alpm_list.h>

{# import Distribution.ArchLinux.ALPM.Types #}

import Control.Monad

import Foreign
import Foreign.C

newtype ALPMList a = ALPMList (Ptr (ALPMList a))

unALPMList :: ALPMList a -> Ptr (ALPMList a)
unALPMList (ALPMList ptr) = ptr 

{- TODO
 - Bind the free callback function with some allocation handling.
-}

-- Generic Function Bindings --------------------------------------------------

type ALPMListSort a b = a -> b -> Int 
foreign import ccall "wrapper" 
  mkALPMListSort :: (Ptr a -> Ptr b -> CInt) 
                 -> IO (FunPtr (Ptr a -> Ptr b -> CInt))

alpmListSortWrapper :: (ALPMType a,ALPMType b) => ALPMListSort a b -> Ptr a -> Ptr b -> CInt
alpmListSortWrapper f c1 c2 =
  fromIntegral $ f (pack c1) (pack c2)

-- List Mutaters --------------------------------------------------------------

addALPMList :: ALPMType a =>  ALPMList a -> a -> IO (ALPMList a)
addALPMList list ptr = 
  liftM ALPMList $ 
    alpm_list_add (unALPMList list) (unpack ptr)

joinALPMList :: ALPMList a -> ALPMList a -> IO (ALPMList a)
joinALPMList lst1 lst2 =
  liftM ALPMList $
    alpm_list_join (unALPMList lst1) (unALPMList lst2)

addSortedALPMList :: ALPMType a => ALPMList a -> a -> ALPMListSort a a -> IO (ALPMList a)
addSortedALPMList list dat sort = do
  csort <- mkALPMListSort (alpmListSortWrapper sort)
  newList <- liftM ALPMList $ alpm_list_add_sorted (unALPMList list) (unpack dat) csort 
  freeHaskellFunPtr csort
  return newList 

mergeALPMList :: ALPMType a => ALPMList a -> ALPMList a -> ALPMListSort a a -> IO (ALPMList a)
mergeALPMList lst1 lst2 sort = do
  csort <- mkALPMListSort (alpmListSortWrapper sort)
  newList <- liftM ALPMList $ alpm_list_mmerge (unALPMList lst1) (unALPMList lst2) csort
  freeHaskellFunPtr csort
  return newList 

-- Item Mutators -------------------------------------------------------------

msortALPMList :: ALPMType a => ALPMList a -> Int -> ALPMListSort a a -> IO (ALPMList a)
msortALPMList list n sort = do
  csort <- mkALPMListSort (alpmListSortWrapper sort)
  newList <- liftM ALPMList $ alpm_list_msort (unALPMList list) (fromIntegral n) csort
  freeHaskellFunPtr csort
  return newList 

removeItemALPMList :: ALPMList a -> ALPMList a -> IO (ALPMList a)
removeItemALPMList lst1 lst2 = do
  liftM ALPMList $ alpm_list_remove_item (unALPMList lst1) (unALPMList lst2)

{-
alpm_list_t *alpm_list_remove(alpm_list_t *haystack, const void *needle, alpm_list_fn_cmp fn, void **data);
alpm_list_t *alpm_list_remove_str(alpm_list_t *haystack, const char *needle, char **data);
-}

removeDupesALPMList :: ALPMList a -> IO (ALPMList a)
removeDupesALPMList = liftM ALPMList . alpm_list_remove_dupes . unALPMList

strdupALPMList :: ALPMList a -> IO (ALPMList a)
strdupALPMList = liftM ALPMList . alpm_list_strdup . unALPMList

copyALPMList :: ALPMList a -> IO (ALPMList a)
copyALPMList =  liftM ALPMList . alpm_list_copy . unALPMList

-- alpm_list_t *alpm_list_copy_data(const alpm_list_t *list, size_t size);

reverseALPMList :: ALPMList a -> IO (ALPMList a)
reverseALPMList = liftM ALPMList . alpm_list_reverse . unALPMList


-- Item Accessors -------------------------------------------------------------

firstALPMList :: ALPMList a -> IO (ALPMList a)
firstALPMList = liftM ALPMList . alpm_list_first . unALPMList

nthALPMList :: ALPMList a -> Int -> IO (ALPMList a)
nthALPMList lst n = liftM ALPMList $
  alpm_list_nth (unALPMList lst) (fromIntegral n)

nextALPMList :: ALPMList a -> IO (ALPMList a)
nextALPMList = liftM ALPMList . alpm_list_next . unALPMList

lastALPMList :: ALPMList a -> IO (ALPMList a)
lastALPMList = liftM ALPMList . alpm_list_last . unALPMList

getDataALPMList :: ALPMType a => ALPMList a -> IO a
getDataALPMList = liftM pack . alpm_list_getdata . unALPMList

-- Misc -----------------------------------------------------------------------

countALPMList :: ALPMList a -> IO Int
countALPMList = liftM fromIntegral . alpm_list_count . unALPMList

{-
void *alpm_list_find(const alpm_list_t *haystack, const void *needle, alpm_list_fn_cmp fn);
void *alpm_list_find_ptr(const alpm_list_t *haystack, const void *needle);
char *alpm_list_find_str(const alpm_list_t *haystack, const char *needle);
alpm_list_t *alpm_list_diff(const alpm_list_t *lhs, const alpm_list_t *rhs, alpm_list_fn_cmp fn);
void alpm_list_diff_sorted(const alpm_list_t *left, const alpm_list_t *right,
		alpm_list_fn_cmp fn, alpm_list_t **onlyleft, alpm_list_t **onlyright);
-}

foreign import ccall safe "alpm_list.h alpm_list_add"
  alpm_list_add :: Ptr (ALPMList a) -> Ptr a -> IO (Ptr (ALPMList a))

foreign import ccall safe "alpm_list.h alpm_list_join"
  alpm_list_join :: Ptr (ALPMList a) -> Ptr (ALPMList a) -> IO (Ptr (ALPMList a))

foreign import ccall safe "alpm_list.h alpm_list_add_sorted"
  alpm_list_add_sorted :: Ptr (ALPMList a) -> Ptr a -> FunPtr (Ptr a -> Ptr a -> CInt) -> IO (Ptr (ALPMList a))

foreign import ccall safe "alpm_list.h alpm_list_mmerge"
  alpm_list_mmerge :: Ptr (ALPMList a) -> Ptr (ALPMList a) -> FunPtr (Ptr a -> Ptr a -> CInt) -> IO (Ptr (ALPMList a))

foreign import ccall safe "alpm_list.h alpm_list_remove_dupes"
  alpm_list_remove_dupes :: Ptr (ALPMList a) -> IO (Ptr (ALPMList a))

foreign import ccall safe "alpm_list.h alpm_list_strdup"
  alpm_list_strdup :: Ptr (ALPMList a) -> IO (Ptr (ALPMList a))

foreign import ccall safe "alpm_list.h alpm_list_copy" 
  alpm_list_copy :: Ptr (ALPMList a) -> IO (Ptr (ALPMList a))

foreign import ccall safe "alpm_list.h alpm_list_reverse" 
  alpm_list_reverse :: Ptr (ALPMList a) -> IO (Ptr (ALPMList a))

foreign import ccall safe "alpm_list.h alpm_list_first" 
  alpm_list_first :: Ptr (ALPMList a) -> IO (Ptr (ALPMList a))

foreign import ccall safe "alpm_list.h alpm_list_next" 
  alpm_list_next :: Ptr (ALPMList a) -> IO (Ptr (ALPMList a))

foreign import ccall safe "alpm_list.h alpm_list_last" 
  alpm_list_last :: Ptr (ALPMList a) -> IO (Ptr (ALPMList a))

foreign import ccall safe "alpm_list.h alpm_list_nth" 
  alpm_list_nth :: Ptr (ALPMList a) -> CInt -> IO (Ptr (ALPMList a))

foreign import ccall safe "alpm_list.h alpm_list_getdata" 
  alpm_list_getdata :: Ptr (ALPMList a) -> IO (Ptr a)

foreign import ccall safe "alpm_list.h alpm_list_count" 
  alpm_list_count :: Ptr (ALPMList a) -> IO  CInt 

foreign import ccall safe "alpm_list.h alpm_list_msort" 
  alpm_list_msort :: Ptr (ALPMList a) -> CInt -> FunPtr (Ptr a -> Ptr a -> CInt) -> IO (Ptr (ALPMList a))

foreign import ccall safe "alpm_list.h alpm_list_remove_item" 
  alpm_list_remove_item :: Ptr (ALPMList a) -> Ptr (ALPMList a) -> IO (Ptr (ALPMList a))
