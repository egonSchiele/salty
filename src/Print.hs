module Print where

print2 :: String -> String -> String -> String
print2 str a b = print2_ str (Just a) (Just b)
print2_"" _ _ = ""
print2_ (x:str) (Just a) b
  | x == '%' = a ++ (print2_ str Nothing b)
  | otherwise = x:(print2_ str (Just a) b)
print2_ (x:str) Nothing (Just b)
  | x == '%' = b ++ str
  | otherwise = x:(print2_ str Nothing (Just b))

print3 :: String -> String -> String -> String -> String
print3 str a b c = print3_ str (Just a) (Just b) (Just c)
print3_"" _ _ _ = ""
print3_ (x:str) (Just a) b c
  | x == '%' = a ++ (print3_ str Nothing b c)
  | otherwise = x:(print3_ str (Just a) b c)
print3_ (x:str) Nothing (Just b) c
  | x == '%' = b ++ (print3_ str Nothing Nothing c)
  | otherwise = x:(print3_ str Nothing (Just b) c)
print3_ (x:str) Nothing Nothing (Just c)
  | x == '%' = c ++ str
  | otherwise = x:(print3_ str Nothing Nothing (Just c))

print4 :: String -> String -> String -> String -> String -> String
print4 str a b c d = print4_ str (Just a) (Just b) (Just c) (Just d)
print4_"" _ _ _ _ = ""
print4_ (x:str) (Just a) b c d
  | x == '%' = a ++ (print4_ str Nothing b c d)
  | otherwise = x:(print4_ str (Just a) b c d)
print4_ (x:str) Nothing (Just b) c d
  | x == '%' = b ++ (print4_ str Nothing Nothing c d)
  | otherwise = x:(print4_ str Nothing (Just b) c d)
print4_ (x:str) Nothing Nothing (Just c) d
  | x == '%' = c ++ (print4_ str Nothing Nothing Nothing d)
  | otherwise = x:(print4_ str Nothing Nothing (Just c) d)
print4_ (x:str) Nothing Nothing Nothing (Just d)
  | x == '%' = d ++ str
  | otherwise = x:(print4_ str Nothing Nothing Nothing (Just d))

print5 :: String -> String -> String -> String -> String -> String -> String
print5 str a b c d e = print5_ str (Just a) (Just b) (Just c) (Just d) (Just e)
print5_"" _ _ _ _ _ = ""
print5_ (x:str) (Just a) b c d e
  | x == '%' = a ++ (print5_ str Nothing b c d e)
  | otherwise = x:(print5_ str (Just a) b c d e)
print5_ (x:str) Nothing (Just b) c d e
  | x == '%' = b ++ (print5_ str Nothing Nothing c d e)
  | otherwise = x:(print5_ str Nothing (Just b) c d e)
print5_ (x:str) Nothing Nothing (Just c) d e
  | x == '%' = c ++ (print5_ str Nothing Nothing Nothing d e)
  | otherwise = x:(print5_ str Nothing Nothing (Just c) d e)
print5_ (x:str) Nothing Nothing Nothing (Just d) e
  | x == '%' = d ++ (print5_ str Nothing Nothing Nothing Nothing e)
  | otherwise = x:(print5_ str Nothing Nothing Nothing (Just d) e)
print5_ (x:str) Nothing Nothing Nothing Nothing (Just e)
  | x == '%' = e ++ str
  | otherwise = x:(print5_ str Nothing Nothing Nothing Nothing (Just e))

print6 :: String -> String -> String -> String -> String -> String -> String -> String
print6 str a b c d e f = print6_ str (Just a) (Just b) (Just c) (Just d) (Just e) (Just f)
print6_"" _ _ _ _ _ _ = ""
print6_ (x:str) (Just a) b c d e f
  | x == '%' = a ++ (print6_ str Nothing b c d e f)
  | otherwise = x:(print6_ str (Just a) b c d e f)
print6_ (x:str) Nothing (Just b) c d e f
  | x == '%' = b ++ (print6_ str Nothing Nothing c d e f)
  | otherwise = x:(print6_ str Nothing (Just b) c d e f)
print6_ (x:str) Nothing Nothing (Just c) d e f
  | x == '%' = c ++ (print6_ str Nothing Nothing Nothing d e f)
  | otherwise = x:(print6_ str Nothing Nothing (Just c) d e f)
print6_ (x:str) Nothing Nothing Nothing (Just d) e f
  | x == '%' = d ++ (print6_ str Nothing Nothing Nothing Nothing e f)
  | otherwise = x:(print6_ str Nothing Nothing Nothing (Just d) e f)
print6_ (x:str) Nothing Nothing Nothing Nothing (Just e) f
  | x == '%' = e ++ (print6_ str Nothing Nothing Nothing Nothing Nothing f)
  | otherwise = x:(print6_ str Nothing Nothing Nothing Nothing (Just e) f)
print6_ (x:str) Nothing Nothing Nothing Nothing Nothing (Just f)
  | x == '%' = f ++ str
  | otherwise = x:(print6_ str Nothing Nothing Nothing Nothing Nothing (Just f))
