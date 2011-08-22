object automata {
  type Automaton = (String, Set[String], Map[String, Map[Char, String]])

  val cadr = ("init", Set("end"), Map(
    "init" -> Map('c' -> "more"),
    "more" -> Map('a' -> "more", 'd' -> "more", 'r' -> "end"),
    "end" -> Map[Char, String]()
  ))

  val c_ad_r = ("init", Set("end"), Map(
    "init" -> Map('c' -> "req"),
    "req" -> Map('a' -> "more", 'd' -> "more"),
    "more" -> Map('a' -> "more", 'd' -> "more", 'r' -> "end"),
    "end" -> Map[Char, String]()
  ))

  def run(automaton: Automaton)(input: String): Boolean = {
    val (init, finals, map) = automaton
    def next(state: String, index: Int): Boolean = {
      if (index == input.length) 
	finals.contains(state)
      else
	map.get(state).flatMap(_.get(input(index))) match {
	  case None => false
	  case Some(new_state) => next(new_state, index + 1)
	}
    }
    next(init, 0)
  }

  import cafebabe._
  import AbstractByteCodes._
  import ByteCodes._

  // Compiles the given automaton to a JVM program.
  def compile(className: String, automaton: Automaton): Unit = {
    val classFile = new ClassFile(className, None)

    // Compiles a main method which repeatedly prompts for an input,
    // printing whether it is accepted by the compiled automaton.
    def compileMain(): Unit = {
      val ch = classFile.addMainMethod.codeHandler

      val top = ch.getFreshLabel("top")
      val end = ch.getFreshLabel("end")

      ch << Label(top)

      ch << InvokeStatic("java/lang/System", "console", "()Ljava/io/Console;")
      ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
      ch << Ldc(">> ")
      ch << InvokeVirtual("java/io/PrintStream", "print", "(Ljava/lang/String;)V")
      ch << InvokeVirtual("java/io/Console", "readLine", "()Ljava/lang/String;")
      ch << DUP << IfNull(end)

      ch << InvokeStatic(className, "eval", "(Ljava/lang/String;)Z")
      ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
      ch << SWAP
      ch << InvokeVirtual("java/io/PrintStream", "println", "(Z)V")
      
      ch << Goto(top)

      ch << Label(end)
      ch << POP
      ch << RETURN
      ch.freeze
    }
    compileMain()

    // Compiles a static method which takes an input and returns
    // whether it is accepted by the compiled automaton.
    def compileEval(): Unit = {
      val mh = classFile.addMethod("Z", "eval", "Ljava/lang/String;")
      mh.setFlags(Flags.METHOD_ACC_STATIC)
      val ch = mh.codeHandler

      val input = 0
      val index = ch.getFreshVar
      ch << Ldc(0)
      ch << IStore(index)

      val (init, finals, map) = automaton

      val start = ch.getFreshLabel("start")
      val reject = ch.getFreshLabel("reject")
      val accept = ch.getFreshLabel("accept")

      ch << Goto(start)

      ch << Label(reject) << Ldc(0) << IRETURN
      ch << Label(accept) << Ldc(1) << IRETURN

      for ((state, table) <- map) {
	ch << Label(state)
	ch << ILoad(index) << Ldc(1) << IADD << IStore(index)
	if (state == init) ch << Label(start)
	ch << ALoad(input)
	ch << InvokeVirtual("java/lang/String", "length", "()I")
	ch << ILoad(index)
	ch << If_ICmpEq(if (finals.contains(state)) accept else reject)
	val inputChar = if (table.size == 0) None else {
	  ch << ALoad(input) << ILoad(index)
	  ch << InvokeVirtual("java/lang/String", "charAt", "(I)C")
	  if (table.size == 1) None else {
	    val tempVar = ch.getFreshVar
	    ch << IStore(tempVar)
	    Some(tempVar)
	  }
	}
	for ((char, new_state) <- table) {
	  inputChar.foreach(ch << ILoad(_))
	  ch << Ldc(char)
	  ch << If_ICmpEq(new_state)
	}
	inputChar.foreach(ch.freeVar(_))
	ch << Goto(reject)
      }

      ch.freeze
    }
    compileEval()
    
    classFile.writeToFile(className + ".class")
  }
}

