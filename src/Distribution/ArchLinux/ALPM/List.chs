{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="alpm" prefix="alpm" #}


module Distribution.ArchLinux.ALPM.List where

#include <alpm_list.h>

{# import Distribution.ArchLinux.ALPM.Types #}


import Foreign
import Foreign.C

newtype ALPMList a = ALPMList (Ptr (ALPMList a))

{# pointer *alpm_list_t as ALPMList newtype  nocode#}

unALPMList :: ALPMList a -> Ptr (ALPMList a)
unALPMList (ALPMList ptr) = ptr 

{- TODO
 - Bind the free callback function with some allocation handling.
-}

-- Generic Function Bindings --------------------------------------------------

type ALPMListSort a = a -> a -> Int 
foreign import ccall "wrapper" 
  mkALPMListSort :: (Ptr a -> Ptr a -> CInt) 
                 -> IO (FunPtr (Ptr a -> Ptr a -> CInt))

alpmListSortWrapper :: ALPMType a => ALPMListSort a -> Ptr a -> Ptr a -> CInt
alpmListSortWrapper f c1 c2 =
  fromIntegral $ f (pack c1) (pack c2)

-- List Mutaters --------------------------------------------------------------

addALPMList :: ALPMType a =>  ALPMList a -> a -> ALPMList a
addALPMList list ptr = 
  ALPMList $ 
    alpm_list_add (unALPMList list) (unpack ptr)

joinALPMList :: ALPMList a -> ALPMList a -> ALPMList a
joinALPMList lst1 lst2 =
  ALPMList $
    alpm_list_join (unALPMList lst1) (unALPMList lst2)

addSortedALPMList :: ALPMType a => ALPMList a -> a -> ALPMListSort a -> ALPMList a
addSortedALPMList list dat sort = unsafePerformIO $ do
  csort <- mkALPMListSort (alpmListSortWrapper sort)
  newList <- return .  ALPMList $ alpm_list_add_sorted (unALPMList list) (unpack dat) csort 
  freeHaskellFunPtr csort
  return newList 

mergeALPMList :: ALPMType a => ALPMList a -> ALPMList a -> ALPMListSort a -> ALPMList a 
mergeALPMList lst1 lst2 sort = unsafePerformIO $ do
  csort <- mkALPMListSort (alpmListSortWrapper sort)
  newList <- return . ALPMList $ alpm_list_mmerge (unALPMList lst1) (unALPMList lst2) csort
  freeHaskellFunPtr csort
  return newList 

{-
/* item mutators */
alpm_list_t *alpm_list_msort(alpm_list_t *list, size_t n, alpm_list_fn_cmp fn);
alpm_list_t *alpm_list_remove_item(alpm_list_t *haystack, alpm_list_t *item);
alpm_list_t *alpm_list_remove(alpm_list_t *haystack, const void *needle, alpm_list_fn_cmp fn, void **data);
alpm_list_t *alpm_list_remove_str(alpm_list_t *haystack, const char *needle, char **data);
-}

removeDupesALPMList :: ALPMList a -> ALPMList a
removeDupesALPMList = ALPMList . alpm_list_remove_dupes . unALPMList

strdupALPMList :: ALPMList a -> ALPMList a
strdupALPMList = ALPMList . alpm_list_strdup . unALPMList

copyALPMList :: ALPMList a -> ALPMList a
copyALPMList =  ALPMList . alpm_list_copy . unALPMList

-- alpm_list_t *alpm_list_copy_data(const alpm_list_t *list, size_t size);

reverseALPMList :: ALPMList a -> ALPMList a
reverseALPMList = ALPMList . alpm_list_reverse . unALPMList


-- Item Accessors -------------------------------------------------------------

firstALPMList :: ALPMList a -> ALPMList a
firstALPMList = ALPMList . alpm_list_first . unALPMList

nthALPMList :: ALPMList a -> Int -> ALPMList a
nthALPMList lst n = ALPMList $
  alpm_list_nth (unALPMList lst) (fromIntegral n)

nextALPMList :: ALPMList a -> ALPMList a
nextALPMList = ALPMList . alpm_list_next . unALPMList

lastALPMList :: ALPMList a -> ALPMList a
lastALPMList = ALPMList . alpm_list_last . unALPMList

getDataALPMList :: ALPMType a => ALPMList a -> a
getDataALPMList = pack . alpm_list_getdata . unALPMList

-- Misc -----------------------------------------------------------------------

countALPMList :: ALPMList a -> Int
countALPMList = fromIntegral . alpm_list_count . unALPMList

{-
void *alpm_list_find(const alpm_list_t *haystack, const void *needle, alpm_list_fn_cmp fn);
void *alpm_list_find_ptr(const alpm_list_t *haystack, const void *needle);
char *alpm_list_find_str(const alpm_list_t *haystack, const char *needle);
alpm_list_t *alpm_list_diff(const alpm_list_t *lhs, const alpm_list_t *rhs, alpm_list_fn_cmp fn);
void alpm_list_diff_sorted(const alpm_list_t *left, const alpm_list_t *right,
		alpm_list_fn_cmp fn, alpm_list_t **onlyleft, alpm_list_t **onlyright);
-}

foreign import ccall safe "alpm_list.h alpm_list_add"
  alpm_list_add :: Ptr (ALPMList a) -> Ptr a -> Ptr (ALPMList a)

foreign import ccall safe "alpm_list.h alpm_list_join"
  alpm_list_join :: Ptr (ALPMList a) -> Ptr (ALPMList a) -> Ptr (ALPMList a)

foreign import ccall safe "alpm_list.h alpm_list_add_sorted"
  alpm_list_add_sorted :: Ptr (ALPMList a) -> Ptr a -> FunPtr (Ptr a -> Ptr a -> CInt) -> Ptr (ALPMList a)

foreign import ccall safe "alpm_list.h alpm_list_mmerge"
  alpm_list_mmerge :: Ptr (ALPMList a) -> Ptr (ALPMList a) -> FunPtr (Ptr a -> Ptr a -> CInt) -> Ptr (ALPMList a)

foreign import ccall safe "alpm_list.h alpm_list_remove_dupes"
  alpm_list_remove_dupes :: Ptr (ALPMList a) -> Ptr (ALPMList a)

foreign import ccall safe "alpm_list.h alpm_list_strdup"
  alpm_list_strdup :: Ptr (ALPMList a) -> Ptr (ALPMList a)

foreign import ccall safe "alpm_list.h alpm_list_copy" 
  alpm_list_copy :: Ptr (ALPMList a) -> Ptr (ALPMList a)

-- alpm_list_t *alpm_list_copy_data(const alpm_list_t *list, size_t size);

foreign import ccall safe "alpm_list.h alpm_list_reverse" 
  alpm_list_reverse :: Ptr (ALPMList a) -> Ptr (ALPMList a)

foreign import ccall safe "alpm_list.h alpm_list_first" 
  alpm_list_first :: Ptr (ALPMList a) -> Ptr (ALPMList a)

foreign import ccall safe "alpm_list.h alpm_list_next" 
  alpm_list_next :: Ptr (ALPMList a) -> Ptr (ALPMList a)

foreign import ccall safe "alpm_list.h alpm_list_last" 
  alpm_list_last :: Ptr (ALPMList a) -> Ptr (ALPMList a)

foreign import ccall safe "alpm_list.h alpm_list_nth" 
  alpm_list_nth :: Ptr (ALPMList a) -> CInt ->  Ptr (ALPMList a)

foreign import ccall safe "alpm_list.h alpm_list_getdata" 
  alpm_list_getdata :: Ptr (ALPMList a) -> Ptr a

foreign import ccall safe "alpm_list.h alpm_list_count" 
  alpm_list_count :: Ptr (ALPMList a) -> CInt 
