object errorEstimation {
  class Estimate(val x: Double, val e: Double) {
    def + (that: Estimate) =
      new Estimate(this.x + that.x, this.e + that.e)
    def - (that: Estimate) =
      new Estimate(this.x - that.x, this.e + that.e)
    def * (that: Estimate) =
      new Estimate(this.x * that.x,
                   this.x.abs*that.e+that.x.abs*this.e+this.e*that.e)
    def / (that: Estimate) =
      new Estimate(this.x/that.x, 
                   (this.x.abs*that.e+that.x.abs*this.e)/(that.x.abs*(that.x.abs-that.e)))
    def +- (e2: Double) =
      new Estimate(x,e+e2)
    override def toString =
      x + " +- " + e
  }
  implicit def double2estimate(x: Double): Estimate = new Estimate(x,0)
  implicit def int2estimate(x: Int): Estimate = new Estimate(x,0)

  def main(args: Array[String]) = {
    println(({ x: Estimate => x+2*x+3*x*x})(1 +- 0.1))
    // 6.0 +- 0.93
    println(({ x: Estimate => 
               val y = x+x
               y*y + 2 })(1 +- 0.1))
    // 6.0 +- 0.84
    def poly(x: Estimate) = x+2*x+3/(x*x)
    println(poly(3.0 +- 0.1))
    // 9.33333 +- 0.3242352
    println(poly(30271.3 +- 0.0001))
    // 90813.9 +- 0.0003
    println(((x: Estimate) => poly(x*x))(3 +- 1.0))
    // 27.037 +- 20.931
  }
}
