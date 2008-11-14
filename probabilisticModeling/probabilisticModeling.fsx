#light

// ----------------------------
// Listing 9-7.


type Distribution<'a> =
    abstract Sample : 'a
    abstract Support : Set<'a>
    abstract Expectation: ('a -> float) -> float

let always x =
    { new Distribution<'a> with
         member d.Sample = x
         member d.Support = Set.singleton x
         member d.Expectation(H) = H(x) }

let rnd = System.Random()

let coinFlip (p:float) (d1:Distribution<'a>) (d2:Distribution<'a>) =
    if p < 0.0 || p > 1.0 then failwith "invalid probability in coinFlip"
    { new Distribution<'a> with
         member d.Sample =
             if rnd.NextDouble() < p then d1.Sample else d2.Sample
         member d.Support = d1.Support + d2.Support
         member d.Expectation(H) =
             p * d1.Expectation(H) + (1.0-p) * d2.Expectation(H) }

// ----------------------------
// Listing 9-8.

let bind (dist:Distribution<'a>) (k: 'a -> Distribution<'b>) =
    { new Distribution<'b> with
         member d.Sample = (k(dist.Sample)).Sample
         member d.Support = Set.union_all (dist.Support |> Set.map(fun d -> (k d).Support))
         member d.Expectation(H) = dist.Expectation(fun x -> (k x).Expectation(H)) }

type DistributionBuilder() =
    member x.Delay(f) = bind (always ()) f
    member x.Let(v,f) = bind (always v) f
    member x.Bind(d,f) = bind d f
    member x.Return(x) = always x

let dist = new DistributionBuilder()

// ----------------------------
// Listing 9-9.

let weightedCases (inp: ('a * float) list) =
    let rec coinFlips w l =
        match l with
        | []          -> failwith "no coinFlips"
        | [(d,_)]     -> always d
        | (d,p)::rest -> coinFlip (p/(1.0-w)) (always d) (coinFlips (w+p) rest)
    coinFlips 0.0 inp

let countedCases inp =
    let total = List.sum_by (fun (_,v) -> v) inp
    weightedCases (inp |> List.map (fun (x,v) -> (x,(float v/float total))))

type Outcome = Even | Odd | Zero
let roulette = countedCases [ Even,18; Odd,18; Zero,1]


// ----------------------------

roulette.Sample
// val it : Outcome = Odd
roulette.Sample
// val it : Outcome = Odd
roulette.Expectation (function Even -> 10.0 | Odd -> 0.0 | Zero -> 0.0)
// val it : float = 4.864864865

// ----------------------------

type Light =
    | Red
    | Green
    | Yellow

let trafficLightD = weightedCases [ Red,0.50; Yellow,0.10; Green, 0.40 ]

type Action = Stop | Drive

let cautiousDriver light =
    dist { match light with
           | Red -> return Stop
           | Yellow -> return! weightedCases [ Stop, 0.9; Drive, 0.1 ]
           | Green -> return Drive }

let aggressiveDriver light =
    dist { match light with
           | Red    -> return! weightedCases [ Stop, 0.9; Drive, 0.1 ]
           | Yellow -> return! weightedCases [ Stop, 0.1; Drive, 0.9 ]
           | Green  -> return Drive }

let otherLight light =
    match light with
    | Red -> Green
    | Yellow -> Red
    | Green -> Red
    
type CrashResult = Crash | NoCrash

let crash(driverOneD,driverTwoD,lightD) =
    dist { // Sample from the traffic light
           let! light = lightD

           // Sample the first driver's behavior given the traffic light
           let! driverOne = driverOneD light
           
           // Sample the second driver's behavior given the traffic light
           let! driverTwo = driverTwoD (otherLight light)

           // Work out the probability of a crash
           match driverOne, driverTwo with
             | Drive,Drive -> return! weightedCases [ Crash, 0.9; NoCrash, 0.1 ]
             | _ -> return NoCrash }

let model = crash(cautiousDriver,aggressiveDriver,trafficLightD)

let model2 = crash(aggressiveDriver,aggressiveDriver,trafficLightD)

// ----------------------------

model.Sample
// val it : CrashResult = NoCrash
model.Sample
// val it : CrashResult = NoCrash
model.Expectation(function Crash -> 1.0 | NoCrash -> 0.0)
// val it : float = 0.0369
model2.Expectation(function Crash -> 1.0 | NoCrash -> 0.0)
// val it : float = 0.0891

// ----------------------------
