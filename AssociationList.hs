{-
  Name: Alexander Caines
  Project: AssociationList

  Defining functions for association list mutation and iteration
-}

module AssociationList where

{-
  Parameters: key, defaultValue, aList
  Returns: Returns the value associated to the key or the defaultValue
  Action: Expects a key, a default value, and an a-list as arguments.
    If the key is present, returns the value associated with it.
    Otherwise, returns the default value.
-}
get :: Ord a => a -> b -> [(a,b)] -> b
get key defaultValue [] = defaultValue
get key defaultValue ((a,b) : xs)
    | key == a = b
    | otherwise = get key defaultValue xs

{-
  Parameters: key, aList
  Returns: Element associated with key
  Action: If the key is present, returns the list without the key/value pair.
    Otherwise, returns the list.
-}
pop :: Ord a => a-> [(a,b)] -> [(a,b)]
pop key [] = []
pop key ((a,b):xs)
    | key == a = xs
    | otherwise = (a,b) : pop key xs

{-
  Parameters: key, value, aList
  Returns: the given aList with an inserted (key, value)
  Action: If the key is present, returns the list with the
   value replacing the value previously associated with the key.
   Otherwise, returns the list with the new key/value pair.
-}
put :: Ord a => a -> b -> [(a,b)] -> [(a,b)]
put key value [] = [(key,value)]
put key value ((a,b) : xs)
    | key == a = ((key, value) : xs)
    | otherwise = (a,b) : put key value xs
