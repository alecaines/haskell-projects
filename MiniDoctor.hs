{-
File: MiniDoctor.hs
Author: Ken Lambert
Purpose: provides a reply function to respond to a patient
inputs.
-}

module MiniDoctor (reply) where

import qualified Data.Char (toLower)
import AssociationList (get)

replacements = zip ["i", "me", "my", "you", "your"]
                   ["you", "you", "your", "I", "my"]

-- Replies to a sentence by changing persons.
-- and prepending a qualifier.
-- Calling form: reply sentence
-- Example call:
-- reply "My mother hates me"
-- "Why do you say that your mother hates you?"
reply :: String -> String
reply sentence = "Why do you say that " ++ changePerson sentence ++ "?"

-- Switches personal pronouns in a sentence
-- Calling form: changePerson sentence
-- Example call:
-- changePerson "My mother hates me"
-- "your mother hates you"
changePerson :: String -> String
changePerson sentence = unwords (map myLookup (words sentence))
    where
    myLookup :: String -> String
    myLookup word = get (toLower word) word replacements


-- Converts a string to lowercase.
-- Calling form: toLower anyString
-- Example call:
-- toLower "My mother hates me"
-- "my mother hates me"
toLower :: String -> String
toLower [] = []
toLower (x:xs) = Data.Char.toLower x : toLower xs
