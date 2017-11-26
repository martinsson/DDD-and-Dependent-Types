
palindrome : String -> Bool
palindrome s = case length s < 10 of 
                True => False
                False => reverse (toLower s) == (toLower s)



