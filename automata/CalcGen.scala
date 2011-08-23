object CalcGen {
  val className = "Calc"
  var errors = 0

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

    def compile(classFile: ClassFile, funmap: Map[String, Fun]): Unit = {
      println("Compiling " + name)

      val mh = classFile.addMethod("I", name, "I" * numParams)
      mh.setFlags(Flags.METHOD_ACC_STATIC)
      val ch = mh.codeHandler
      val top = "_top_"

      def c(last: Boolean)(code: Any): Unit = {
	code match {
	  case n: Int => ch << Ldc(n)
	  case v: String => params.indexOf(v) match {
	    case -1 => { errors += 1; println("Error: undefined variable " + v) }
	    case i => ch << ILoad(i)
	  }
	  case List("if", cond, cons, alt) => {
	    c(false)(cond)
	    val if_true = ch.getFreshLabel("if_true")
	    val end = ch.getFreshLabel("end")
	    ch << IfNe(if_true)
	    c(last)(alt)
	    ch << Goto(end)
	    ch << Label(if_true)
	    c(last)(cons)
	    ch << Label(end)
	  }
	  case ("if" :: _) => { errors += 1; println("Error: malformed if") }
	  case ((funName : String) :: args) => funmap.get(funName) match {
	    case None => { errors += 1; println("Error: unknown function " + funName) }
	    case Some(fun) => if (args.size != fun.numParams) { errors += 1; println("Error: wrong number of arguments for function " + funName + ". Expected " + fun.numParams + " not " + args.size + ".") } else {
	      args.foreach(c(false))
	      if (!last || funName != name) {
		fun.invoke(ch)		
	      } else {
		for (i <- (0 until numParams).reverse)
		  ch << IStore(i)
		ch << Goto(top)
	      }
	    }
	  }
	  case _ => { errors += 1; println("Error: malformed expression") }
	}
      }
      ch << Label(top)
      c(true)(source)
      ch << IRETURN
      if (errors == 0) ch.freeze
    }

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

  object PrimitiveEq extends PrimitiveCmp("=", 2, If_ICmpEq(_))

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
	case e => { errors += 1; println("Error parsing " + filename); println(e); List() }
      }
    }
  }

  def main(args: Array[String]) {
    val defs = args.flatMap(parser.parseDefinitions(_))

    val funmap = Map() ++ ((primitives ++ defs).map(f => (f.name, f)))

    val classFile = new ClassFile(className, None)

    defs.foreach(_.compile(classFile, funmap))

    if (errors > 0) println("There were errors.")
    else classFile.writeToFile(className + ".class")
  }
}
