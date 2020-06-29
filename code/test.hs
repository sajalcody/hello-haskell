sayHello :: String -> IO()
sayHello x = 
    putStrLn ("Hello, " ++ x ++ "!!")

thrice :: Num a => a -> a
thrice x = x * 3

--circleArea :: Num a => a -> a
circleArea r = pi * (r * r)