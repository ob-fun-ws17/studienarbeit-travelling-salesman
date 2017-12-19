module Lib
    (
    bruteForceEntryPoint
    ) where

distance :: (Num,Num) (Num,Num) -> Num
distance (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
  where
    x' = x1 - x2
    y' = y1 - y2


createDistMatrix :: Array Int (Int,Int) -> Array (Int,Int) Num
createDistMatrix coordinates = DistMatrix
  where
    DistMatrix = array((1,1)(lengthInput,lengthInput))[distance coordinates!i coordinates!j | i <- [1..lengthInput] | j <- [1..lengthInput]]
    lengthInput = snd bounds coordinates


bruteForceRecursion :: (Array (Int,Int) Num) (Num,Array Int (Int,Int)) (Array Int Int) (Array Int (Int,Int)) Int -> (Num,Array Int (Int,Int))
bruteForceRecursion distMatrix currentResult nodesToVisit coordinates endNode =


bruteForceEntryPoint :: (Array Int (Int,Int)) Int -> (Num,Array Int (Int,Int))
bruteForceEntryPoint coordinates startNode = bruteForceRecursion distMatrix firstResult nodesToVisit coordinates endNode
  where
    endNode = startNode
    distMatrix = createDistMatrix coordinates
    firstResult = (0, array (0,1)[coordinates!startNode])
    nodesToVisit =
