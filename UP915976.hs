--------------------------------------------------------------------------------
---------------------------Functional Programming-------------------------------
--------------------------------------------------------------------------------
------------------------------- UP915976----------------------------------------


import Data.Foldable (minimumBy)
import Data.Ord (comparing)
data Place = Place {name :: String,
                    lat :: Float,
                    long :: Float,
                    rainfall :: [Int]
                    } deriving (Eq, Ord, Show)

testData :: [Place]
testData = [Place "London"     51.5 (-0.1) [0, 0, 5, 8, 8, 0, 0],
            Place "Cardiff"    51.5 (-3.2) [12, 8, 15, 0, 0, 0, 2],
            Place "Norwich"    52.6 (1.3)  [0, 6, 5, 0, 0, 0, 3],
            Place "Birmingham" 52.5 (-1.9) [0, 2, 10, 7, 8, 2, 2],
            Place "Liverpool"  53.4 (-3.0) [8, 16, 20, 3, 4, 9, 2],
            Place "Hull"       53.8 (-0.3) [0, 6, 5, 0, 0, 0, 4],
            Place "Newcastle"  55.0 (-1.6) [0, 0, 8, 3, 6, 7, 5],
            Place "Belfast"    54.6 (-5.9) [10, 18, 14, 0, 6, 5, 2],
            Place "Glasgow"    55.9 (-4.3) [7, 5, 3, 0, 6, 5, 0],
            Place "Plymouth"   50.4 (-4.1) [4, 9, 0, 0, 0, 6, 5],
            Place "Aberdeen"   57.1 (-2.1) [0, 0, 6, 5, 8, 2, 0],
            Place "Stornoway"  58.2 (-6.4) [15, 6, 15, 0, 0, 4, 2],
            Place "Lerwick"    60.2 (-1.1) [8, 10, 5, 5, 0, 0, 3],
            Place "St Helier"  49.2 (-2.1) [0, 0, 0, 0, 6, 10, 0]
            ]
--------------------------------------------------------------------------------
---------------------------------------1----------------------------------------
--------------------------------------------------------------------------------
getNames :: [Place] -> [String]
getNames [] = []
getNames (x:z) = name x : getNames z
--------------------------------------------------------------------------------
---------------------------------------2----------------------------------------
--------------------------------------------------------------------------------

getRainAvg :: String -> [Place] -> Float
getRainAvg _ [] = 0
getRainAvg n (x:z)
    |n == name x = listAverage (rainfall x)
    | otherwise = 0 + getRainAvg n z

listAverage :: [Int] -> Float
listAverage (x:z) = Main.trunk(
                       fromIntegral(foldr (+) x z) / fromIntegral(length (x:z))
                       ) 2

trunk :: Float -> Int -> Float
trunk x n = (fromIntegral (floor (x * t))) / t
    where t = 10^n

--------------------------------------------------------------------------------
---------------------------------------3----------------------------------------
--------------------------------------------------------------------------------

getPlaceAndRainfall :: [Place] -> String
getPlaceAndRainfall [] = []
getPlaceAndRainfall (x:z) = name x ++" "++show(lat x)++" "++show(long x)++" ["++
                        stringifyList(rainfall x)++"]"++"\n"++getPlaceAndRainfall z

stringifyList :: [Int] -> String
stringifyList [] = []
stringifyList (x:z)
    | length z == 0 = show x
    | otherwise = show x ++", "++stringifyList z

--------------------------------------------------------------------------------
---------------------------------------4----------------------------------------
--------------------------------------------------------------------------------
findDryspell :: Int -> [Place] -> [String]
findDryspell 0 _ = []
findDryspell _ [] = []
findDryspell dayNum (x:z)
    | (rainfall x)!!(dayNum-1) == 0 = name x:findDryspell dayNum z
    | otherwise = findDryspell dayNum z
--------------------------------------------------------------------------------
---------------------------------------5----------------------------------------
--------------------------------------------------------------------------------

modifyData :: [Int] -> [Place] -> [Place]
modifyData [] _ = []
modifyData (u:us) (x:z) = (Place (name x) (lat x) (long x)
                           (updateListItems u (rainfall x))):modifyData us z

updateListItems :: Int -> [Int] -> [Int]
updateListItems _ [] = []
updateListItems n (x:z)
    | z == [] = n:updateListItems n z
    | otherwise = x:updateListItems n z

--------------------------------------------------------------------------------
---------------------------------------6----------------------------------------
--------------------------------------------------------------------------------
changePlace :: String -> Place -> [Place] -> [Place]
changePlace _ _ [] = []
changePlace targetName replacement (x:z)
    | name x == targetName = replacement:changePlace targetName replacement z
    | otherwise = x:changePlace targetName replacement z

--------------------------------------------------------------------------------
---------------------------------------7----------------------------------------
--------------------------------------------------------------------------------

findClosestDry :: Float -> Float -> [Place] -> Place
findClosestDry x y places = fst $ minimumBy (comparing snd) (distancesTo x y places)

dryPrevious :: [Place] -> [Place]
dryPrevious [] = []
dryPrevious (x:z)
    | rainfall x!!0 == 0 = x:dryPrevious z
    | otherwise = dryPrevious z

distancesTo :: Float -> Float -> [Place] -> [(Place, Float)]
distancesTo x y = map (\place -> (place, proximity x y place))

proximity :: Float -> Float -> Place -> Float
proximity x y place = calculateDistance (x) (y) (lat place) (long place)

calculateDistance :: Float -> Float -> Float -> Float -> Float
calculateDistance x1 y1 x2 y2 = sqrt ((y1 - y2)^2 + (x1 - x2)^2)

--------------------------------------------------------------------------------
-----------------------------------Demos----------------------------------------
--------------------------------------------------------------------------------

demo :: Int -> IO ()
demo 1 = putStrLn (show (getNames testData))
demo 2 = putStrLn (show (getRainAvg "Cardiff" testData))
demo 3 = putStrLn (getPlaceAndRainfall testData)
demo 4 = putStrLn (show (findDryspell 2 testData))
demo 5 = putStrLn (show (modifyData [0,8,0,0,5,0,0,3,4,2,0,8,0,0] testData))
demo 6 = putStrLn (show (changePlace "Plymouth"
                  (Place "Portsmouth" 50.8  (-1.1)  [0, 0, 3, 2, 5, 2, 1]) testData))
demo 7 = putStrLn (show (findClosestDry 50.9 (-1.3) (dryPrevious testData)))
demo 8 = draw testData

--------------------------------------------------------------------------------
-------------------------------Rainfall Map-------------------------------------
--------------------------------------------------------------------------------
type ScreenPosition = (Int,Int)

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
moveTo :: ScreenPosition -> IO ()
moveTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
placeString :: ScreenPosition -> String -> IO ()
placeString position text = do
    moveTo position
    putStr text

draw :: [Place] -> IO ()
draw list = do
    drawCorners
    markPlaces testData
    moveTo (0,51)

markPlaces :: [Place] -> IO ()
markPlaces [] = putStr("")
markPlaces (x:z) = do
    placeString (getX (long x),getY (lat x)) ("+ "++nameAndAv x)
    markPlaces z

getY :: Float -> Int
getY y = 50 - (round ((y-49)/0.4)) -12
getX :: Float -> Int
getX x = 80 + (round ((x-7)/0.14)) +22

drawCorners :: IO ()
drawCorners = do
    clearScreen
    placeString (0,0) "x"
    placeString (80,0) "x"
    placeString (0,50) "x"
    placeString (80,50) "x"

nameAndAv :: Place -> String
nameAndAv place = name place ++ " " ++
                  (show (getRainAvg (name place) testData))

myRepeat :: [Int] -> IO ()
myRepeat [] = putStr("")
myRepeat (x:z) = do
    putStrLn(show x)
    myRepeat z
