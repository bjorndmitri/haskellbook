eratosthenes :: Int -> [Int]
-- returns the set of primes leq the input using the sieve of eratosthenes
eratosthenes n = reverse $ go ([2..n], []) where
    go :: ([Int], [Int]) -> [Int]
    go ([], x) = x
    go ((x:xs), y) = go (crossOut x xs, x:y)

crossOut :: Int -> [Int] -> [Int]
crossOut prime list = filter (\x -> x `mod` prime /= 0) list
