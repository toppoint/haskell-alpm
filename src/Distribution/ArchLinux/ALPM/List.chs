{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="alpm" prefix="alpm" #}


module Distribution.ArchLinux.ALPM.List where


#include <alpm_list.h>

{# import Distribution.ArchLinux.ALPM.Types #}

import Foreign
import Foreign.C

{# pointer *alpm_list_t as ALPMList newtype #}

{- TODO
   Bind the free callback function with Foreign.Marshal.Alloc.free 
   to free the CStrings we allocate for the list.
-}

-- Generic Function Bindings --------------------------------------------------

type ALPMListSort = String -> String -> Int 
foreign import ccall "wrapper" 
  mkALPMListSort :: (Ptr () -> Ptr () -> IO CInt) 
                 -> IO (FunPtr (Ptr () -> Ptr () -> IO CInt))

alpmListSortWrapper :: ALPMListSort -> Ptr () -> Ptr () -> IO CInt
alpmListSortWrapper f cstr1 cstr2 = do
  str1 <- peekCString $ castPtr cstr1 
  str2 <- peekCString $ castPtr cstr2
  return . fromIntegral $ f str1 str2

-- List Mutaters --------------------------------------------------------------

addALPMList :: ALPMList -> String -> IO ALPMList
addALPMList list str = 
  {# call list_add #} list . castPtr =<< newCString str

joinALPMList :: ALPMList -> ALPMList -> IO ALPMList
joinALPMList lst1 lst2 =
  {#call list_join #} lst1 lst2

addSortedALPMList :: ALPMList -> String -> ALPMListSort -> IO ALPMList 
addSortedALPMList list str sort = do
  csort <- mkALPMListSort (alpmListSortWrapper sort)
  cstr <- newCString str
  {# call list_add_sorted #} list (castPtr cstr) csort 
  -- TODO free function pointer
 
mergeALPMList :: ALPMList -> ALPMList -> ALPMListSort -> IO ALPMList
mergeALPMList lst1 lst2 sort = do
  csort <- mkALPMListSort (alpmListSortWrapper sort)
  {# call list_mmerge #} lst1 lst2 csort 
  -- TODO free function pointer

{-
/* item mutators */
alpm_list_t *alpm_list_msort(alpm_list_t *list, size_t n, alpm_list_fn_cmp fn);
alpm_list_t *alpm_list_remove_item(alpm_list_t *haystack, alpm_list_t *item);
alpm_list_t *alpm_list_remove(alpm_list_t *haystack, const void *needle, alpm_list_fn_cmp fn, void **data);
alpm_list_t *alpm_list_remove_str(alpm_list_t *haystack, const char *needle, char **data);
alpm_list_t *alpm_list_remove_dupes(const alpm_list_t *list);
alpm_list_t *alpm_list_strdup(const alpm_list_t *list);
-}

copyALPMList :: ALPMList -> IO ALPMList
copyALPMList = {# call list_copy #} 

{-
alpm_list_t *alpm_list_copy_data(const alpm_list_t *list, size_t size);
alpm_list_t *alpm_list_reverse(alpm_list_t *list);
-}


-- Item Accessors -------------------------------------------------------------

firstALPMList :: ALPMList -> IO ALPMList
firstALPMList list =
  {# call list_first #} list

nthALPMList :: ALPMList -> Int -> IO ALPMList
nthALPMList lst n =
  {# call list_nth #} lst $ fromIntegral n 

nextALPMList :: ALPMList -> IO ALPMList
nextALPMList lst =
  {# call list_next #} lst

lastALPMList :: ALPMList -> IO ALPMList
lastALPMList lst =
  {# call list_last #} lst 

getDataALPMList :: ALPMList -> IO String
getDataALPMList lst =
  {# call list_getdata #} lst >>= (peekCString . castPtr)


-- Misc -----------------------------------------------------------------------
{-
/* misc */
size_t alpm_list_count(const alpm_list_t *list);
void *alpm_list_find(const alpm_list_t *haystack, const void *needle, alpm_list_fn_cmp fn);
void *alpm_list_find_ptr(const alpm_list_t *haystack, const void *needle);
char *alpm_list_find_str(const alpm_list_t *haystack, const char *needle);
alpm_list_t *alpm_list_diff(const alpm_list_t *lhs, const alpm_list_t *rhs, alpm_list_fn_cmp fn);
void alpm_list_diff_sorted(const alpm_list_t *left, const alpm_list_t *right,
		alpm_list_fn_cmp fn, alpm_list_t **onlyleft, alpm_list_t **onlyright);
-}
