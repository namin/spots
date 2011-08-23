object CalcGen {
  val baseClassName = "CalcBase"
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

  def compile(classFile: ClassFile, funmap: Map[String, Fun]): Unit = {
    classFile.addDefaultConstructor

    def compileMain(): Unit = {
      val ch = classFile.addMainMethod.codeHandler
      ch << DefaultNew(className)
      ch << InvokeVirtual(className, "readEvalLoop", "()V");
      ch << RETURN
      ch.freeze
    }
    compileMain()

    def compileDispatch(): Unit = {
      val ch = classFile.addMethod("Z", "dispatch", "Ljava/lang/String;").codeHandler

      val bad = "_bad_"
      val print = "_print_"
      val push = "_push_"
      val input = 1

      for (fun <- funmap.values) {
	val next = ch.getFreshLabel("next")
	ch << ALoad(input)
	ch << Ldc(fun.name)
	ch << InvokeVirtual("java/lang/String", "equals", "(Ljava/lang/Object;)Z")
	ch << IfEq(next)
	ch << ALoad(0) << GetField(className, "stack", "Ljava/util/Stack;")
	ch << InvokeVirtual("java/util/Stack", "size", "()I")
	ch << Ldc(fun.numParams)
	ch << ISUB
	ch << DUP
	ch << IfLt(bad)
	val index = ch.getFreshVar
	ch << IStore(index)
	(0 until fun.numParams).reverse.foreach(_ => {
	  ch << ALoad(0) << GetField(className, "stack", "Ljava/util/Stack;")
	  ch << ILoad(index)
	  ch << InvokeVirtual("java/util/Stack", "remove", "(I)Ljava/lang/Object;")
	  ch << CheckCast("java/lang/Integer")
	  ch << InvokeVirtual("java/lang/Integer", "intValue", "()I")
	})
	ch.freeVar(index)
	fun.invoke(ch)
	ch << Goto(push)
	ch << Label(next)
      }

      ch << Ldc("!!! undefined operation") << Goto(print)
      ch << Label(bad) << POP << Ldc("!!! stack too short")
      ch << Label(print)
      ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
      ch << SWAP
      ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
      ch << Ldc(0) << IRETURN
      ch << Label(push)
      ch << InvokeStatic("java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;")
      ch << ALoad(0) << GetField(className, "stack", "Ljava/util/Stack;")
      ch << SWAP
      ch << InvokeVirtual("java/util/Stack", "addElement", "(Ljava/lang/Object;)V")
      ch << Ldc(1) << IRETURN

      ch.freeze
    }
    compileDispatch()
  }

  def main(args: Array[String]) {
    val defs = args.flatMap(parser.parseDefinitions(_))

    val funmap = Map() ++ ((primitives ++ defs).map(f => (f.name, f)))

    val classFile = new ClassFile(className, Some(baseClassName))

    defs.foreach(_.compile(classFile, funmap))

    if (errors > 0) println("There were errors.") else {
      compile(classFile, funmap)
      classFile.writeToFile(className + ".class")
      println("Done")
    }
  }
}
