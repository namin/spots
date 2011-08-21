object CalcGen {
  import cafebabe._
  import AbstractByteCodes._
  import ByteCodes._

  def compile() {
    val classFile = new ClassFile("Calc", None)
    val ch = classFile.addMainMethod.codeHandler

    ch << RETURN
    ch.freeze
    classFile.writeToFile(classFile.className + ".class")
  }

  def main(args: Array[String]) {
    compile()
  }
}
