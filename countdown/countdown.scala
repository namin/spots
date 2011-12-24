import scala.util.Random
import scala.collection.immutable.Stack

object countdown {
  val numbers = (1 to 10) ++ (1 to 10) ++ List(25, 50, 75, 100)

  def generateProblem(): (Seq[Int], Int) = {
    val r = new Random()
    ((1 to 6 map (_ => numbers(r.nextInt(numbers.length)))).toList, 101 + r.nextInt(899))
  }

  sealed trait Op {
    def valid(x: Int, y: Int): Boolean = this match {
      case Plus | Mul => x <= y
      case Sub => x > y
      case Div => x > y && x % y == 0
    }

    def eval(x: Int, y: Int): Int = this match {
      case Plus => x + y
      case Sub => x - y
      case Mul => x * y
      case Div => x / y
    }

    override def toString(): String = this match {
      case Plus => "+"
      case Sub => "-"
      case Mul => "*"
      case Div => "/"
    }
  }

  case object Plus extends Op
  case object Sub extends Op
  case object Mul extends Op
  case object Div extends Op

  val ops = List(Plus, Sub, Mul, Div)

  sealed trait Expr {
    val value: Int
  }

  case class Value(value: Int) extends Expr {
    override def toString(): String = value.toString()
  }

  case class App(op: Op, x: Expr, y: Expr) extends Expr {
    val value = op.eval(x.value, y.value)

    override def toString(): String = {
      x.toString() + "\n" + y.toString() + "\n" + x.value.toString() + " " + op.toString() + " " + y.value.toString() + " = " + value.toString()
    }
  }

  def updated[A](s: Seq[A], el: A, i: Int, j: Int): Seq[A] = {
    val (ip, jp) = if (i < j) (i, j) else (j, i)
    s.slice(0, ip) ++ s.slice(ip + 1, jp) ++ s.slice(jp + 1, s.length) ++ List(el)
  }

  def solveProblem(problem: (Seq[Int], Int)): Option[App] = {
    val (p, goal) = problem
    var stack : Stack[Seq[Expr]] = Stack.Empty
    stack = stack.push(p.map(Value(_)))
    while (!stack.isEmpty) {
      val s = stack.top
      stack = stack.pop
      for (i <- 0 to s.length - 1) {
	val x = s(i)
	for (j <- 0 to s.length - 1) {
	  val y = s(j)
	  if (i != j) {
	    for (op <- ops) {
	      if (op.valid(x.value, y.value)) {
		val e = App(op, x, y)
		if (e.value == goal) {
		  return Some(e)
		}
		stack = stack.push(updated(s, e, i, j))
	      }
	    }
	  }
	}
      }
    }
    return None
  }

  def solveApproxProblem(problem: (Seq[Int], Int)): (Int, Expr) = {
    val (p, goal) = problem

    def updatedBest(best: (Int, Expr), expr: Expr): (Int, Expr) = {
      val (bestDiff, _) = best
      val diff = Math.abs(goal - expr.value)
      if (diff < bestDiff) (diff, expr) else best
    }
    var stack : Stack[Seq[Expr]] = Stack.Empty
    stack = stack.push(p.map(Value(_)))

    var best = (goal, Value(0) : Expr)
    for (v  <- stack.top) {
      best = updatedBest(best, v)
    }

    while (!stack.isEmpty) {
      val s = stack.top
      stack = stack.pop
      for (i <- 0 to s.length - 1) {
	val x = s(i)
	for (j <- 0 to s.length - 1) {
	  val y = s(j)
	  if (i != j) {
	    for (op <- ops) {
	      if (op.valid(x.value, y.value)) {
		val e = App(op, x, y)
		if (e.value == goal) {
		  return (0, e)
		}
		best = updatedBest(best, e)
		stack = stack.push(updated(s, e, i, j))
	      }
	    }
	  }
	}
      }
    }
    return best
  }

  def main(args: Array[String]): Unit = {
    val problem = generateProblem()
    val (p, goal) = problem
    println("Find " + goal.toString() + " from " + p.mkString(", ") + ".")
    val (diff, expr) = solveApproxProblem(problem)
    println(if (diff == 0) "Solved exactly." else ("Solved within +/- " + diff + "."))
    println(expr.toString())
  }
}
