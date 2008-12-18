import System.Random

-- Support is omitted, because I don't know how to integrate the (Ord a) 
-- constraint of the underlying set when creating a monad.

data Sample a = 
    Sample (IO a)

data Expectation a =
    Expectation ((a -> Double) -> Double)

data Distribution a = 
    Distribution (Sample a) (Expectation a)

always x = 
    Distribution (Sample sample) (Expectation expectation)
    where sample = return x
          expectation h = h(x)

rnd :: IO Double
rnd = getStdRandom (randomR (0.0,1.0))

coinFlip p 
         (Distribution (Sample sample1) (Expectation expectation1)) 
         (Distribution (Sample sample2) (Expectation expectation2)) =
    Distribution (Sample sample) (Expectation expectation)
    where sample = do rndProb <- rnd; if rndProb < p then sample1 else sample2
          expectation h = p * expectation1(h) + (1.0-p) * expectation2(h)

distSample (Distribution (Sample sample) (Expectation expectation)) = sample
distExpectation (Distribution (Sample sample) (Expectation expectation)) = expectation

(|>) x f = f x

bind dist k =
    Distribution (Sample sample) (Expectation expectation)
    where sample = do d <- dist |> distSample; k d |> distSample
          expectation h = (dist |> distExpectation)(\x -> ((k x) |> distExpectation)(h))

instance Monad Distribution where
    (>>=) = bind
    return = always

weightedCases inp =
    coinFlips 0.0 inp
    where coinFlips w l = 
              case l of
                [] -> error "no coinFlips"
                [(d,_)] -> always d
                (d,p):rest -> coinFlip (p/(1.0-w)) (always d) (coinFlips (w+p) rest)

countedCases inp =
    weightedCases (inp |> map (\(x,v) -> (x,v/total)))
    where total = 1.0*(inp |> map (\(_,v) -> v) |> sum)

data Outcome = Even | Odd | Zero deriving (Show,Eq,Ord)

roulette = countedCases [(Even,18),(Odd,18),(Zero,1)]

printSample d =
    do r <- distSample d
       putStrLn $ show $ r

printExpectation d h =
    putStrLn $ show $ (distExpectation d) h

data Light = Red | Green | Yellow deriving (Show,Eq,Ord)

trafficLightD = weightedCases [(Red,0.50),(Yellow,0.10),(Green,0.40)]

data Action = Stop | Drive deriving (Show,Eq,Ord)

cautiousDriver light =
    case light of
      Red -> always Stop
      Yellow -> weightedCases [(Stop,0.9),(Drive,0.1)]
      Green -> always Drive

aggressiveDriver light =
    case light of
      Red -> weightedCases [(Stop,0.9),(Drive,0.1)]
      Yellow -> weightedCases [(Stop,0.1),(Drive,0.9)]
      Green -> always Drive

otherLight light =
    case light of
      Red -> Green
      Yellow -> Red
      Green -> Red

data CrashResult = Crash | NoCrash deriving (Show,Eq,Ord)

crash driverOneD driverTwoD lightD =
    do light <- lightD
       driverOne <- driverOneD light
       driverTwo <- driverTwoD (otherLight light)
       case (driverOne,driverTwo) of
         (Drive,Drive) -> weightedCases [(Crash,0.9),(NoCrash,0.1)]
         _ -> return NoCrash

model = crash cautiousDriver aggressiveDriver trafficLightD

model2 = crash aggressiveDriver aggressiveDriver trafficLightD

main =
    do printSample roulette 
       -- Odd
       printSample roulette 
       -- Even
       printExpectation roulette (\x -> case x of
                                          Even -> 10.0
                                          Odd -> 0.0
                                          Zero -> 0.0)
       -- 4.864864864864865
       printSample model
       -- NoCrash
       printSample model
       -- NoCrash
       printExpectation model (\x -> case x of
                                       Crash -> 1.0
                                       NoCrash -> 0.0)
       -- 0.036899999999999995
       printExpectation model2 (\x -> case x of
                                        Crash -> 1.0
                                        NoCrash -> 0.0)
       -- 0.08909999999999998
