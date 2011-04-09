{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="alpm" prefix="alpm" #}


module Distribution.ArchLinux.ALPM.List where


#include <alpm_list.h>

{# import Distribution.ArchLinux.ALPM.Types #}

import Foreign
import Foreign.C

{# pointer *alpm_list_t as ALPMList newtype #}

-- List Mutaters ---------------------------------------------------------------

{-
/* item mutators */
alpm_list_t *alpm_list_add_sorted(alpm_list_t *list, void *data, alpm_list_fn_cmp fn);
alpm_list_t *alpm_list_mmerge(alpm_list_t *left, alpm_list_t *right, alpm_list_fn_cmp fn);
alpm_list_t *alpm_list_msort(alpm_list_t *list, size_t n, alpm_list_fn_cmp fn);
alpm_list_t *alpm_list_remove_item(alpm_list_t *haystack, alpm_list_t *item);
alpm_list_t *alpm_list_remove(alpm_list_t *haystack, const void *needle, alpm_list_fn_cmp fn, void **data);
alpm_list_t *alpm_list_remove_str(alpm_list_t *haystack, const char *needle, char **data);
alpm_list_t *alpm_list_remove_dupes(const alpm_list_t *list);
alpm_list_t *alpm_list_strdup(const alpm_list_t *list);
alpm_list_t *alpm_list_copy(const alpm_list_t *list);
alpm_list_t *alpm_list_copy_data(const alpm_list_t *list, size_t size);
alpm_list_t *alpm_list_reverse(alpm_list_t *list);
-}

addALPMList :: ALPMList -> String -> IO ALPMList
addALPMList list str = 
-- alpm_list_t *alpm_list_add(alpm_list_t *list, void *data);
  {# call list_add #} list . castPtr =<< newCString str

joinALPMList :: ALPMList -> ALPMList -> IO ALPMList
joinALPMList lst1 lst2 =
-- alpm_list_t *alpm_list_join(alpm_list_t *first, alpm_list_t *second);
  {#call list_join #} lst1 lst2


-- Item Accessors --------------------------------------------------------------

{- /* item accessors */
alpm_list_t *alpm_list_first(const alpm_list_t *list);
alpm_list_t *alpm_list_nth(const alpm_list_t *list, size_t n);
alpm_list_t *alpm_list_next(const alpm_list_t *list);
alpm_list_t *alpm_list_last(const alpm_list_t *list);
void *alpm_list_getdata(const alpm_list_t *entry);
-}

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
