{-
File: Student.hs

Alexander Caines

Represents a student with a name and a set of scores.
-}

module Student where

data Student = Student String [Int]
  deriving (Show)

--Creates and returns new Student type variable
--given a String name and Integer representing
--the maximum number of scores in a list
newStudent :: String -> Int -> Student
newStudent name numberOfScores =
  let scores = map (\x -> 0) [1..numberOfScores]
  in Student name scores

-- Returns the number of scores for this student
getNumberOfScores :: Student -> Int
getNumberOfScores (Student _ scores) = length scores

--Returns the name value of a Student type variable
getName :: Student -> String
getName (Student name _) = name

--Returns a new Student with the old Student's
--scores and a new name
setName :: String -> Student -> Student
setName newName (Student _ scores) =
  Student newName scores

-- Returns the student's score at the given index,
-- counting from 0
getScore :: Int -> Student -> Int
getScore index (Student name scores)
    | index == 0 = head scores
    | otherwise = getScore (index - 1) (Student name (tail scores))

-- Resets the student's score at the given index,
-- counting from 0, and returns the student
setScore :: Int -> Int -> Student -> Student
setScore index newScore (Student name scores) = (Student name ((replace (index) newScore scores)))

--Helper function to setScore. Returns a new list
--with a new score in the place of an old one
replace :: Int -> Int -> [Int] -> [Int]
replace index newScore (x:xs)
    | index == 0 = newScore:xs
    | otherwise = x:replace (index-1) (newScore) (xs)

-- Returns the student's highest score
getHighScore :: Student -> Int
getHighScore (Student name [x]) = x
getHighScore (Student name (x:xs))
    | x > head xs = getHighScore (Student name (x : (tail xs)))
    | otherwise = getHighScore (Student name xs)


-- Returns the student's average score as a Float
getAverageScore :: Student -> Float
getAverageScore (Student _ scores) = fromIntegral (sum scores) / fromIntegral (length scores)
