module Week1
  ( N
  , Theta
  , TrainingSet
  , LearnRate
  , hypothesis
  , cost
  , cost'
  , gradientDescent
  ) where

type N = Double
type Theta = (N, N)
type TrainingSet = [(N, N)]
type LearnRate = N

hypothesis :: Theta -> N -> N
hypothesis (t0, t1) x = t0 + t1 * x

-- 'Squared error function' or 'Mean squared error'
cost :: Theta -> TrainingSet -> N
cost theta ts = (1 / (2 * m)) * summation ts (\(x, y) -> (hypothesis theta x - y) ^ 2)
  where
    m = fromIntegral (length ts)

summation :: Num b => [a] -> (a -> b) -> b
summation xs f = sum (map f xs)

-- Derived cost function
cost' :: Theta -> TrainingSet -> N
cost' theta ts = (1 / m) * summation ts (\(x, y) -> hypothesis theta x - y)
  where
    m = fromIntegral (length ts)

-- Gradient Descent for Linear Regression
gradientDescent :: LearnRate -> Theta -> TrainingSet -> Theta
gradientDescent rate theta ts = converge rate theta ts
  where
    converge :: LearnRate -> Theta -> TrainingSet -> Theta
    converge rate theta@(t0, t1) ts = let
      t0' = t0 - rate * cost' theta ts
      t1' = t1 - rate * cost' theta ts
      theta' = (t0', t1')
      in if near t0 t0' && near t1 t1'
        then converge rate theta' ts
        else theta'

-- Near zero
near :: N -> N -> Bool
near x y = nearZero (x - y)

nearZero :: N -> Bool
nearZero x = abs x <= epsilon

epsilon :: N
epsilon = 0.0000000001
