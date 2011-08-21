object CalcGen {
  import cafebabe._
  import AbstractByteCodes._
  import ByteCodes._

  def compile() {
    val classFile = new ClassFile("Calc", None)
    val ch = classFile.addMainMethod.codeHandler

    val top = ch.getFreshLabel("top")
    val end = ch.getFreshLabel("end")

    ch << Label(top)
    ch << InvokeStatic("java/lang/System", "console", "()Ljava/io/Console;")
    ch << InvokeVirtual("java/io/Console", "readLine", "()Ljava/lang/String;")
    ch << DUP << IfNull(end)

    ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
    ch << DUP
    ch << Ldc("... ")
    ch << InvokeVirtual("java/io/PrintStream", "print", "(Ljava/lang/String;)V")
    ch << SWAP
    ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")

    ch << Goto(top)

    ch << Label(end)
    ch << POP
    ch << RETURN
    ch.freeze
    classFile.writeToFile(classFile.className + ".class")
  }

  def main(args: Array[String]) {
    compile()
  }
}
