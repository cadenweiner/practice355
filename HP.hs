--Caden Weiner
--practice higher order functions, using annonymous functions, currying etc. 

module HP     where
    -- tail recursion
    addup [] acc = acc
    addup (x:xs) acc = addup xs (acc + x) 



    map' op [] = [] 
    map' op (x:xs) = (op x) : (map' op xs)
    
    allSquared xL = map square xL
                      where 
                          square x = x * x


    sq = \x -> x *x -- function that takes one operator and adds 20 to the value
    addtwo = \x y -> x + y -- two param anonymous function
    addsquare = \x y -> sq x + sq y -- we can call functions within anonymous functions


    filter' op [] = [] 
    filter' op (x:xs) |(op x) = x : (filter' op xs) -- does the operation with op
                      |otherwise = filter' op xs -- otherwise always evaluates to true


    -- filter and map seem to be support functions

    negatives [] = [] 
    negatives xL = filter' (\x-> x < 0) xL --fixed it do not treat it as x:xs


    test [] = [] 
    test (x:xs) |(x < 0) = x : (test xs) -- does the operation with op
                |otherwise = test xs -- otherwise always evaluates to true


    greater x y = if x > y then x else y
    maxL xs = foldr greater (minBound::Int) xs -- max for a single list
    maxLL iL = maxL (map maxL iL) -- max for a list with nested lists



    sumofsquareroots [] = 0
    sumofsquareroots (x:xs) 
            | x > 0 = sqrt x + sumofsquareroots xs -- only positives
            |otherwise = sumofsquareroots xs



    sumOfSquareRoots xs = sum $ map sqrt $ filter (\x -> x>0) xs

    sosr = sum . map sqrt . filter (\x -> x>0) 

    data Days = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
                deriving (Show, Eq, Ord)
    isWeekend day |day == Sunday = True 
                  |day == Saturday = True
                  |otherwise = False

    weekday Sunday = False
    weekday Sunday = False
    weekday _ = True -- We don't care what it is we know it is not the weekend


    weekday2 day = not (day `elem` [Saturday, Sunday])


    data Money = NONE | COIN Int | BILL Int -- indicates they will have parameters
                deriving (Eq, Show)

    x = (COIN 10)
    y = (BILL 20)
    z = NONE


    -- patterns are very helpful for these values
    --it is very difficult without patterns

    amount NONE = 0
    amount (COIN v) = v 
    amount (BILL v) = v * 100

    --fromIntegral is needed for division
    dollaramount NONE = fromIntegral(0)
    dollaramount (COIN v) = fromIntegral(v)/100
    dollaramount (BILL v) = fromIntegral(v)

    --OR

    --dollaramount NONE = COIN 0
    --dollaramount (COIN v) = COIN v
    --dollaramount (BILL v) = COIN (v * 100)


    -- we need all posible permutations
    -- addMoney (COIN v) (COIN v)
    -- addMoney (COIN v) (BILL v)
    -- addMoney (COIN v) (NONE v)
    -- addMoney (BILL v) (BILL v)
    -- addMoney (BILL v) (COIN v)
    -- addMoney (BILL v) (NONE v)
    -- addMoney (NONE v) (NONE v)
    -- addMoney (NONE v) (COIN v)
    -- addMoney (NONE v) (BILL v)






