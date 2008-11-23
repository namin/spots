package probabilisticModeling

object probabilisticModeling {
  import scala.collection.immutable.Set1
  import scala.util.Random

  abstract class Distribution[A] {
    def Sample: A
    def Support: Set[A]
    def Expectation(H: A => Double): Double
    def map[B](k: A => B) = flatMap((x: A) => always(k(x)))
    def flatMap[B](k: A => Distribution[B]): Distribution[B] = bind(this)(k)
  }
    
  def always[A](x: A) = new Distribution[A] {
    def Sample = x
    def Support = new Set1(x)
    def Expectation(H: A => Double) = H(x)
  }
  
  val rnd = new Random
  
  def coinFlip[A](p: Double)(d1: Distribution[A])(d2: Distribution[A]) = {
    if (p < 0.0 || p > 1.0) error("invalid probability")
    new Distribution[A] {
      def Sample = 
        if (rnd.nextDouble() < p) d1.Sample else d2.Sample 
      def Support = 
        d1.Support ++ d2.Support
      def Expectation(H : A => Double) = 
        p * d1.Expectation(H) + (1.0-p) * d2.Expectation(H)
    }
  }
    
  def bind[A,B](dist: Distribution[A])(k: A => Distribution[B]): Distribution[B] = new Distribution[B] {
    def Sample =	
      (k(dist.Sample)).Sample
    def Support() = 
      dist.Support.flatMap(k(_).Support)
    def Expectation(H : B => Double) = 
      dist.Expectation(k(_).Expectation(H))
  }

  def weightedCases[A](inp: List[(A,Double)]): Distribution[A] = {
    def coinFlips[A](w: Double)(l: List[(A,Double)]): Distribution[A] = {
      l match {
        case Nil => error("no coinFlips")
        case (d,_)::Nil => always(d)
        case (d,p)::rest => coinFlip(p/(1.0-w))(always(d))(coinFlips(w+p)(rest))
      }
    }
    coinFlips(0)(inp)
  }
 
  def countedCases[A](inp: List[(A,Int)]): Distribution[A] = {
    val total = 1.0*(inp map { case (_,v) => v } reduceLeft (_+_))
    weightedCases(inp map { case (x,v) => (x,v/total) })
  }
  
  sealed trait Outcome
  final case object Even extends Outcome
  final case object Odd extends Outcome
  final case object Zero extends Outcome
  
  val roulette = countedCases(List((Even,18),(Odd,18),(Zero,1)))
    
  val roulettePayoff = 
    roulette.Expectation(x => x match {
      case Even => 10.0
      case Odd => 0.0
      case Zero => 0.0
    }
    )
  
  sealed trait Light
  final case object Red extends Light
  final case object Green extends Light
  final case object Yellow extends Light

  def trafficLightD: Distribution[Light] = weightedCases(List((Red,0.50),(Yellow,0.10),(Green,0.40)))
  
  sealed trait Action
  final case object Stop extends Action
  final case object Drive extends Action
  
  def cautiousDriver(light: Light): Distribution[Action] =
    light match {
      case Red => always(Stop)
      case Yellow => weightedCases(List((Stop,0.9),(Drive,0.1)))
      case Green => always(Drive)
    }
  
  def aggressiveDriver(light: Light): Distribution[Action] =
    light match {
      case Red => weightedCases(List((Stop,0.9),(Drive,0.1)))
      case Yellow => weightedCases(List((Stop,0.1),(Drive,0.9)))
      case Green => always(Drive)
    }
  
  def otherLight(light: Light): Light =
    light match {
      case Red => Green
      case Yellow => Red
      case Green => Red
    }
    
  sealed trait CrashResult
  final case object Crash extends CrashResult
  final case object NoCrash extends CrashResult
    
  def crashExplicit(driverOneD: Light => Distribution[Action])(driverTwoD: Light => Distribution[Action])(lightD: Distribution[Light]): Distribution[CrashResult] =
    lightD.flatMap(light =>
      driverOneD(light).flatMap(driverOne =>
        driverTwoD(otherLight(light)).flatMap(driverTwo =>
          (driverOne, driverTwo) match {
            case (Drive,Drive) => weightedCases(List((Crash,0.9),(NoCrash,0.1)))
            case _ => always(NoCrash)
          })))
  
  def crash(driverOneD: Light => Distribution[Action])(driverTwoD: Light => Distribution[Action])(lightD: Distribution[Light]): Distribution[CrashResult] =
    for (light <- lightD;
         driverOne <- driverOneD(light);
         driverTwo <- driverTwoD(otherLight(light));
         caseBothDrive <- weightedCases(List((Crash,0.9),(NoCrash,0.1)))) yield
    (driverOne,driverTwo) match {
      case (Drive,Drive) => caseBothDrive
      case _ => NoCrash
    }

  val model = crash(cautiousDriver)(aggressiveDriver)(trafficLightD)    
  val model2 = crash(aggressiveDriver)(aggressiveDriver)(trafficLightD)

  def H(x: CrashResult) = x match {
    case Crash => 1.0
    case NoCrash => 0.0
  }

  def main(args: Array[String]) = {
    println("roulette sample: " + roulette.Sample)
    // roulette sample: Odd
    println("roulette sample (again): " + roulette.Sample)
    // roulette sample (again): Even
    println("roulette payoff: " + roulettePayoff)
    // roulette payoff: 4.864864864864865
    println("model sample: " + model.Sample)
    // model sample: NoCrash
    println("model2 sample: " + model2.Sample)
    // model2 sample: NoCrash
    println("model crash expectation: " + model.Expectation(H))
    // model crash expectation: 0.036899999999999995
    println("model2 crash expectation: " + model2.Expectation(H))
    // model2 crash expectation: 0.08909999999999998
  }
}
