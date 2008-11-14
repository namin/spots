package probabilisticModeling

object probabilisticModeling {
  import scala.collection.immutable.Set1
  import scala.util.Random

  abstract class Distribution[A] {
    def Sample(): A
    def Support(): Set[A]
    def Expectation(H: A => Double): Double
    def map[B](k: A => B) = flatMap((x: A) => always(k(x)))
    def flatMap[B](k: A => Distribution[B]): Distribution[B] = bind(this)(k)
  }
    
  def always[A](x: A) = new Distribution[A] {
    def Sample() = x
    def Support() = new Set1(x)
    def Expectation(H: A => Double) = H(x)
  }
  
  val rnd = new Random
  
  def coinFlip[A](p: Double)(d1: Distribution[A])(d2: Distribution[A]) = {
    if (p < 0.0 || p > 1.0) error("invalid probability")
    new Distribution[A] {
      def Sample() = 
        if (rnd.nextDouble() < p) d1.Sample() else d2.Sample() 
      def Support() = 
        d1.Support() ++ d2.Support()
      def Expectation(H : A => Double) = 
        p * d1.Expectation(H) + (1.0-p) * d2.Expectation(H)
    }
  }
    
  def bind[A,B](dist: Distribution[A])(k: A => Distribution[B]): Distribution[B] = new Distribution[B] {
    def Sample() =	
      (k(dist.Sample())).Sample()
    def Support() = 
      dist.Support().flatMap(d => k(d).Support())
    def Expectation(H : B => Double) = 
      dist.Expectation(x => k(x).Expectation(H))
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
    def sumTotal[A](inp: List[(A,Int)]): Int = 
      inp match {
        case Nil => 0
        case (_,n)::rest => n + sumTotal(rest)
      }
    val total = 1.0*(sumTotal(inp))
    weightedCases(inp map (t => t match { case (x,v) => (x,v/total) }))
  }
  
  abstract class Outcome
  case class Even() extends Outcome
  case class Odd() extends Outcome
  case class Zero() extends Outcome
  
  val roulette = countedCases(List((Even(),18),(Odd(),18),(Zero(),1)))
    
  val roulettePayoff = 
    roulette.Expectation(x => x match {
      case Even() => 10.0
      case Odd() => 0.0
      case Zero() => 0.0
    }
    )
  
  abstract class Light
  case class Red() extends Light
  case class Green() extends Light
  case class Yellow() extends Light

  def trafficLightD: Distribution[Light] = weightedCases(List((Red(),0.50),(Yellow(),0.10),(Green(),0.40)))
  
  abstract class Action
  case class Stop() extends Action
  case class Drive() extends Action
  
  def cautiousDriver(light: Light): Distribution[Action] =
    light match {
      case Red() => always(Stop())
      case Yellow() => weightedCases(List((Stop(),0.9),(Drive(),0.1)))
      case Green() => always(new Drive())
    }
  
  def aggressiveDriver(light: Light): Distribution[Action] =
    light match {
      case Red() => weightedCases(List((Stop(),0.9),(Drive(),0.1)))
      case Yellow() => weightedCases(List((Stop(),0.1),(Drive(),0.9)))
      case Green() => always(Drive())
    }
  
  def otherLightFun(light: Light): Light =
    light match {
      case Red() => Green()
      case Yellow() => Red()
      case Green() => Red()
    }
  
  def otherLightDFun(light: Light): Distribution[Light] =
    light match {
      case Red() => weightedCases(List((Green(),0.8),(Yellow(),0.2)))
      case Yellow() => always(Red())
      case Green() => always(Red())
    }
  
  abstract class CrashResult
  case class Crash() extends CrashResult
  case class NoCrash() extends CrashResult
    
  def crashExplicit(driverOneD: Light => Distribution[Action])(driverTwoD: Light => Distribution[Action])(lightD: Distribution[Light]): Distribution[CrashResult] =
    lightD.flatMap(light =>
      driverOneD(light).flatMap(driverOne =>
        otherLightDFun(light).flatMap(otherLight =>
          driverTwoD(otherLight).flatMap(driverTwo =>
            (driverOne, driverTwo) match {
              case (Drive(),Drive()) => weightedCases(List((Crash(),0.9),(NoCrash(),0.1)))
              case _ => always(NoCrash())
            }))))
  
  def crash(driverOneD: Light => Distribution[Action])(driverTwoD: Light => Distribution[Action])(lightD: Distribution[Light]): Distribution[CrashResult] =
    for (light <- lightD;
         driverOne <- driverOneD(light);
         otherLight <- otherLightDFun(light);
         driverTwo <- driverTwoD(otherLight);
         caseBothDrive <- weightedCases(List((Crash(),0.9),(NoCrash(),0.1)))) yield
    (driverOne,driverTwo) match {
      case (Drive(),Drive()) => caseBothDrive
      case _ => NoCrash()
    }

  val model = crash(cautiousDriver)(aggressiveDriver)(trafficLightD)    
  val model2 = crash(aggressiveDriver)(aggressiveDriver)(trafficLightD)

  def H(x: CrashResult) = x match {
    case Crash() => 1.0
    case NoCrash() => 0.0
  }

  def main(args: Array[String]) = {
    println("roulette sample: " + roulette.Sample())
    // roulette sample: Odd()
    println("roulette sample (again): " + roulette.Sample())
    // roulette sample (again): Even()
    println("roulette payoff: " + roulettePayoff)
    // roulette payoff: 4.864864864864865
    println("model sample: " + model.Sample())
    // model sample: NoCrash()
    println("model2 sample: " + model2.Sample())
    // model2 sample: NoCrash()
    println("model crash expectation:" + model.Expectation(H))
    // model crash expectation:0.036899999999999995
    println("model2 crash expectation:" + model2.Expectation(H))
    // model2 crash expectation:0.0882
  }
}
