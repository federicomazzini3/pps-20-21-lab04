package u04lab.code

trait Complex {
  def re: Double

  def im: Double

  def +(c: Complex): Complex // should implement the sum of two complex numbers..
  def *(c: Complex): Complex // should implement the product of two complex numbers
}

object Complex {
  def apply(re: Double, im: Double): Complex = new ComplexImpl(re, im)

  private class ComplexImpl(override val re: Double,
                            override val im: Double) extends Complex {

    override def +(c: Complex): Complex = {
      new ComplexImpl(re + c.re, im + c.im)
    }

    override def *(c: Complex): Complex = {
      new ComplexImpl((re * c.re - im * c.im), (re * c.im + im * c.re))
    }
  }
}

object ComplexWithCase{
  def apply(re:Double, im: Double): Complex = new ComplexWithCaseImpl(re,im)

  case class ComplexWithCaseImpl(re: Double, im: Double) extends Complex{
    override def +(c: Complex): Complex = {
      ComplexWithCase(re + c.re, im + c.im)
    }

    override def *(c: Complex): Complex = {
      ComplexWithCase((re * c.re - im * c.im), (re * c.im + im * c.re))
    }
  }

}




object TryComplex extends App {
  val a = Array(Complex.apply(10, 20), Complex(1, 1), Complex(7, 0))
  val c = a(0).+(a(1)) + a(2)
  println("non case class")
  println(c, c.re, c.im) // (ComplexImpl(18.0,21.0),18.0,21.0)
  val c2 = a(0) * a(1)
  println(c2, c2.re, c2.im) // (ComplexImpl(-10.0,30.0),-10.0,30.0)

  val b = Array(ComplexWithCase(10, 20), ComplexWithCase(1,1), Complex(7,0))
  val d = b(0).+(b(1)) + b(2)
  println("case class")
  println(d, d.re, d.im) // (ComplexImpl(18.0,21.0),18.0,21.0)
  val d2 = b(0) * b(1)
  println(d2, d2.re, d2.im) // (ComplexImpl(-10.0,30.0),-10.0,30.0)
}

/** Hints:
 * - implement Complex with a ComplexImpl class, similar to PersonImpl in slides
 * - check that equality and toString do not work
 * - use a case class ComplexImpl instead, creating objects without the 'new' keyword
 * - check equality and toString now
 */