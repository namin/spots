object CalcGen {
  val className = "Calc"

  import cafebabe._
  import AbstractByteCodes._
  import ByteCodes._

  abstract class Fun {
    val name: String
    val numParams: Int
    def invoke(ch: CodeHandler): Unit
  }

  case class DefFun(override val name: String, val params: List[String], val source: Any) extends Fun {
    override val numParams = params.size
    def invoke(ch: CodeHandler): Unit = {
      ch << InvokeStatic(className, name, "(" + "I" * numParams + ")I")
    }
  }

  abstract case class PrimitiveFun(override val name: String, override val numParams: Int) extends Fun

  object PrimitiveAdd extends PrimitiveFun("+", 2) {
    override def invoke(ch: CodeHandler): Unit = ch << IADD
  }

  object PrimitiveSub extends PrimitiveFun("-", 2) {
    override def invoke(ch: CodeHandler): Unit = ch << ISUB
  }

  object PrimitiveMul extends PrimitiveFun("*", 2) {
    override def invoke(ch: CodeHandler): Unit = ch << IMUL
  }

  abstract class PrimitiveCmp(override val name: String, override val numParams: Int, val cmp: (String) => ControlOperator) extends PrimitiveFun(name, numParams) {
    override def invoke(ch: CodeHandler): Unit = {
      val if_true = ch.getFreshLabel("if_true")
      val end = ch.getFreshLabel("end")
      ch << cmp(if_true)
      ch << Ldc(0) << Goto(end)
      ch << Label(if_true) << Ldc(1)
      ch << Label(end)
    }
  }

  object PrimitiveLt extends PrimitiveCmp("<", 2, If_ICmpLt(_))

  object PrimitiveEq extends PrimitiveCmp("<", 2, If_ICmpEq(_))

  val primitives = List(PrimitiveAdd, PrimitiveSub, PrimitiveMul, PrimitiveLt, PrimitiveEq)

  import scala.util.parsing.combinator._

  object parser extends JavaTokenParsers {
    private def s = """\s*""".r
    private def atom: Parser[String] = """[^\(\)\s]+""".r
    private def number: Parser[Int]  = wholeNumber ^^ { _.toInt }
    private def list: Parser[List[Any]] = 
      '(' ~>s~> rep(expr) <~ ')' <~s
    private def expr: Parser[Any] =
      number <~s | atom <~s | list
    private def definition: Parser[DefFun] =
      defname ~ defargs ~ defbody ^^ { case name~args~body => DefFun(name, args, body) }
    private def defname: Parser[String] =
      '(' ~>s~> "define" ~>s~> '(' ~>s~> ident <~s
    private def defargs: Parser[List[String]] =
      rep(atom) <~ ')' <~s
    private def defbody: Parser[Any] =
      expr <~ ')' <~s
    private def definitions: Parser[List[DefFun]] =
      rep(definition)

    private def content(filename: String): String = {
      scala.io.Source.fromFile(filename).mkString
    }

    def parseDefinitions(filename: String): List[DefFun] = {
      parseAll(definitions, content(filename)) match {
	case Success(defs, _) => { println("Parsed " + filename); defs }
	case e => { println("Error parsing " + filename); println(e); List() }
      }
    }
  }

  def main(args: Array[String]) {
    val defs = args.flatMap(parser.parseDefinitions(_))
    println("Done")
  }
}
