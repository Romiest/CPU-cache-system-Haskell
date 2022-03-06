import Data.Foldable


data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)
data Output a = Out (a, Int) | NoOutput deriving (Show, Eq)

len [] = 0
len (x:xs) = 1 + len xs

square x= x*x
pow a 0 = 1
pow a 1 = a
pow a b= if (even b)==True then square(pow a (div b 2)) else a*pow a (b-1)

convertBinToDec :: Integral a => a -> a



convertBinToDec x = convertBinToDec1 x 0 (mod x 10) 0


convertBinToDec1 0  y  _  m = m
convertBinToDec1 x y z m = convertBinToDec1 (div x 10) (y+1) (mod (div x 10) 10) (m+(z* pow 2 y))     



replaceIthItem :: (Eq a, Num a) => t -> [t] -> a -> [t]

replaceIthItem x y z = replaceIthItem1 x y z 0 



replaceIthItem1  item (x1:xs) index count = if(count==index) then (item:xs) else (x1:replaceIthItem1 item xs index (count+1)) 

logBase2 :: (Floating a) => a -> a


logBase2 x = logBase 2 x




splitEvery :: (Num a1, Ord a1) => a1 -> [a] -> [[a]]


splitEvery num l = splitEvery1 num l 0 []


splitEvery1 num []  _  l= [l]
splitEvery1 num (x1:xs) x l= if(x /= num) then (splitEvery1 num xs (x+1) (l++[x1])) else ([l]++splitEvery1 num (x1:xs) 0 []) 

fillZeros :: (Eq a, Num a) => [Char] -> a -> [Char]


fillZeros l n = if(n /= 0) then (fillZeros (['0']++l) (n-1)) else l





getNumBits :: (Floating a1, Integral a, RealFrac a1, Foldable t) =>a1 -> [Char] -> t  a2 -> a




getNumBits _ "fullyAssoc" _ = 0

getNumBits x "setAssoc" _ = ceiling (logBase2 x)

getNumBits x "directMap" _ = ceiling (logBase2 x)










convertAddress:: (Integral b1, Integral b2) => b1 -> b2 -> p -> (b1, b1)

convertAddress x num  _ = ((div x (pow 10 num)), (mod x (pow 10 num)))












getDataFromCache:: (Integral b, Eq a) => [Char] -> [Item a] -> [Char] -> b -> Output a



getDataFromCache address l x bitsNum   |( x=="fullyAssoc") = getDataFromCache0 address l "fullyAssoc" bitsNum
									   | (x=="directMap") = getDataFromCache2 address l "directMap" bitsNum
									   | (x=="setAssoc")  = getDataFromCache3 address l "setAssoc" bitsNum 
									   | otherwise = error "re enter the type"
							   
							   
							   
getDataFromCache0 stringAddress ( ( It (T x ) (D y) ( b) ( i) ):xs) "fullyAssoc" bitsNum = getDataFromCache1 stringAddress ( ( It (T x) (D y) (b) ( i) )    :xs) "fullyAssoc" bitsNum 0

 
getDataFromCache1 _ [] "fullyAssoc" _ _= NoOutput  
getDataFromCache1 stringAddress ( ( It (T x ) (D y) ( b) ( i) ):xs) "fullyAssoc" bitsNum a=if( (read stringAddress :: Int)==x && b==True) then Out(y,a) else getDataFromCache1  stringAddress xs "fullyAssoc" bitsNum (a+1)   





getDataFromCache2 address cache "directMap" bits = getDataFromCacheDirect(convertAddress (read address :: Int) bits "directMap") cache 



getDataFromCacheDirect (x,y) l = getDataFromItem (l!!(convertBinToDec y)) x


getDataFromItem (It (T tag) (D da) valid order) tag1 | tag==tag1 && valid = Out(da,0)
													 | otherwise = NoOutput













getDataFromCache3 address cache "setAssoc" bits =getDataFromCacheSet (convertAddress (read address :: Int) bits "setAssoc") (getList (splitEvery(div (len cache) (2^bits)) cache) (convertAddress (read address :: Int) bits "setAssoc")) 0



getList l (x,y) = l!!(convertBinToDec(y))

getDataFromCacheSet _ [] _ = NoOutput

getDataFromCacheSet (x,y) (It (T tag) (D da) valid order:xs) num |x==tag && valid = Out(da,num)
															     |otherwise = getDataFromCacheSet (x,y) (xs) (num+1)
																 
																 
																 
																 
																 
																 
replaceInCache :: Integral b => Int -> Int -> [a] -> [Item a] -> [Char] -> b -> (a, [Item a])


replaceInCache  tag idx memory oldCache x bitsNum 			| (x=="directMap") = replaceInCache1  tag idx memory oldCache "directMap" bitsNum
															| (x=="fullyAssoc") = replaceInCache2  tag idx memory oldCache "fullyAssoc" bitsNum
															| (x=="setAssoc")= replaceInCache3  tag idx memory oldCache "setAssoc" bitsNum
															| otherwise = error "re enter the type"
															
															
getAddress tag idx bitsNum = convertBinToDec (read addressString)
    where
        tagString = show tag
        idxTemp = show idx
        zeros = bitsNum - len idxTemp
        idxString = fillZeros idxTemp zeros
        addressString = tagString ++ idxString


		
-- Replace direct associative															
replaceInCache1 tag idx memory oldCache "directMap" bitsNum = (itemData, replaceIthItem newItem oldCache idxDec)
    where
        address = getAddress tag idx bitsNum
        itemData = memory!!address
        newItem = It (T tag) (D itemData) True 0
        idxDec = convertBinToDec idx

--Replace Fully Associative
replaceInCache2 tag _ memory oldCache "fullyAssoc" _ = (itemData, newCache)
   where
       tagDec = convertBinToDec tag
       itemData = memory!!tagDec
       newItem = It (T tag) (D itemData) True 0
       tempCache = add1ToOrder oldCache
       priority = findFIFO tempCache
       newCache = replaceIthItem newItem tempCache priority


--Replace Set Associative
replaceInCache3 tag idx memory oldCache "setAssoc" 0 = replaceInCache3 tag idx memory oldCache "fullyAssoc" 0
replaceInCache3 tag idx memory oldCache "setAssoc" bitsNum = (itemData, newCache)
    where
        address = getAddress tag idx bitsNum
        idxDec = convertBinToDec idx
        itemData = memory!!address
        newItem = It (T tag) (D itemData) True 0
        setsNum = pow 2 bitsNum
        cacheLen = len oldCache
        every = ceiling (cacheLen/setsNum)
        splitCache = splitEvery every oldCache
        targetSet = splitCache!!idxDec
        priority = findFIFO targetSet
        tempSet = add1ToOrder targetSet
        newSet = replaceIthItem newItem tempSet priority
        almostNewCache = replaceIthItem newSet splitCache idxDec
        newCache = flatten almostNewCache



flatten [] = []
flatten (x:xs) = x ++ flatten xs

add1ToOrder [] = []
add1ToOrder ((It t d True order):xs) = (It t d True (order+1)):add1ToOrder xs
add1ToOrder ((It t d False order):xs) = (It t d False (order)):add1ToOrder xs


findFIFO l = findFIFO1 l l 0
findFIFO1 l [] n = findFIFO2 l 0 (-1) (-1)
findFIFO1 l ((It _ _ True _):xs) n = findFIFO1 l xs (n+1)
findFIFO1 l ((It _ _ False _):_) n = n

findFIFO2 [] _ _ maxN = maxN
findFIFO2 ((It _ _ _ order):xs) n maxOrder maxN
  |order >= maxOrder = findFIFO2 xs (n+1) order n
  |otherwise         = findFIFO2 xs (n+1) maxOrder maxN		






getX (Out (d, _)) = d


getData :: (Eq t, Integral b) =>String -> [Item t] -> [t] -> [Char] -> b -> (t, [Item t])


getData stringAddress cache memory cacheType bitsNum | x == NoOutput = replaceInCache tag index memory cache cacheType bitsNum
													 | otherwise = (getX x, cache)
														where
															x = getDataFromCache stringAddress cache cacheType bitsNum
															address = read stringAddress :: Int
															(tag, index) = convertAddress address bitsNum cacheType
											






runProgram:: (RealFloat a1, Eq a2) =>[[Char]] -> [Item a2] -> [a2] -> [Char] -> a1 -> ([a2], [Item a2])


runProgram [] cache _ _ _ = ([], cache)
runProgram (addr: xs) cache memory cacheType numOfSets = ((d:prevData), finalCache)
	where
		bitsNum = round(logBase2 numOfSets)
		(d, updatedCache) = getData addr cache memory cacheType bitsNum
		(prevData, finalCache) = runProgram xs updatedCache memory cacheType numOfSets
  