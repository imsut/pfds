import Criterion.Main
import LeftistHeap
import System.Random

type Rand a = StdGen -> (a, StdGen)

randomInts :: Int -> Rand [Int]
randomInts 0 rng = ([], rng)
randomInts n rng = let (x, rng') = next rng
                       (xs, rng'') = randomInts (n - 1) rng'
                   in (x:xs, rng'')

findMax :: (Int -> LeftistHeap Int -> LeftistHeap Int) ->
           (LeftistHeap Int -> LeftistHeap Int) -> [Int] -> Int
findMax ins del ns = findMax' $ foldr ins empty ns
  where
    findMax' :: LeftistHeap Int -> Int
    findMax' h = let e  = findMin h
                     h' = del h
                 in if isEmpty h' then e else findMax' h'

main :: IO ()
main = do stdGen <- newStdGen
          let (ns, _) = randomInts 1000 stdGen
            in defaultMain [ bench "Left"  $ whnf (findMax insertL deleteMinL) ns,
                             bench "Right" $ whnf (findMax insert deleteMin) ns
                           ]
