import Criterion.Main
import RedBlackSet
import System.Random

type Rand a = StdGen -> (a, StdGen)

randomInts :: Int -> Rand [Int]
randomInts 0 rng = ([], rng)
randomInts n rng = let (x, rng') = next rng
                       (xs, rng'') = randomInts (n - 1) rng'
                   in (x:xs, rng'')

main :: IO ()
main = do stdGen <- newStdGen
          let (ns, _) = randomInts 1000 stdGen
              ms = [1..1000]
            in defaultMain [ bench "random 0" $ whnf (member 0 . foldr insert (empty :: RedBlackSet Int)) ns,
                             bench "random 1" $ whnf (member 0 . foldr insert (empty :: RedBlackSet1 Int)) ns,
                             bench "random 2" $ whnf (member 0 . foldr insert (empty :: RedBlackSet2 Int)) ns,
                             bench "random 3" $ whnf (member 0 . foldr insert (empty :: RedBlackSet3 Int)) ns,
                             bench "sorted 0" $ whnf (member 0 . foldr insert (empty :: RedBlackSet Int)) ms,
                             bench "sorted 1" $ whnf (member 0 . foldr insert (empty :: RedBlackSet1 Int)) ms,
                             bench "sorted 2" $ whnf (member 0 . foldr insert (empty :: RedBlackSet2 Int)) ms,
                             bench "sorted 3" $ whnf (member 0 . foldr insert (empty :: RedBlackSet3 Int)) ms
                           ]
