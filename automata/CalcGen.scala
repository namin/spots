object CalcGen {
  import cafebabe._
  import AbstractByteCodes._
  import ByteCodes._

  def compile() {
    val classFile = new ClassFile("Calc", None)
    val ch = classFile.addMainMethod.codeHandler

    val stack = ch.getFreshVar
    val top = ch.getFreshLabel("top")
    val end = ch.getFreshLabel("end")
    val done = ch.getFreshLabel("done")

    ch << Ldc(0)
    ch << IStore(stack)

    ch << Label(top)
    ch << ILoad(stack) << Ldc(1) << IADD << IStore(stack)
    ch << InvokeStatic("java/lang/System", "console", "()Ljava/io/Console;")
    ch << InvokeVirtual("java/io/Console", "readLine", "()Ljava/lang/String;")
    ch << DUP << IfNull(end)

    ch << DUP
    ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
    ch << DUP
    ch << Ldc("... ")
    ch << InvokeVirtual("java/io/PrintStream", "print", "(Ljava/lang/String;)V")
    ch << InvokeVirtual("java/io/PrintStream", "println", "(I)V")

    ch << Goto(top)

    ch << Label(end)
    ch << ILoad(stack)
    ch << IfEq(done)
    ch << POP
    ch << ILoad(stack) << Ldc(1) << ISUB << IStore(stack)
    ch << Goto(end)

    ch << Label(done)
    ch << RETURN
    ch.freeze
    classFile.writeToFile(classFile.className + ".class")
  }

  def main(args: Array[String]) {
    compile()
  }
}
