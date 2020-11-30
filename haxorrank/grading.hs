
gradingStudents :: [Integer] -> [Integer]
gradingStudents grades =
    map grading grades
    where 
    grading x
          | x < 40 = x -- hm
          | (x `mod` 5) == 3 = x + 2
          | (x `mod` 5) == 4 = x + 1
          | otherwise = x 
