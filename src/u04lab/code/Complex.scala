package u04lab.code

/*
 * ex 1 complex numbers
 */
trait Complex {
  def re: Double

  def im: Double

  def +(c: Complex): Complex // should implement the sum of two complex numbers..
  def *(c: Complex): Complex // should implement the product of two complex numbers
}

/*
 * Implemento Complex con la classe ComplexImpl
 * Essendo ComplexImpl una class e non una case class, dovrò istanziarla con new ComplexImpl(re,im)
 * equals e toString non saranno implementati ad hoc per la classe
 */

  class ComplexImpl(override val re: Double,
                            override val im: Double) extends Complex {

    override def +(c: Complex): Complex = {
      new ComplexImpl(re + c.re, im + c.im)
    }

    override def *(c: Complex): Complex = {
      new ComplexImpl((re * c.re - im * c.im), (re * c.im + im * c.re))
    }
}

/*
 * Implemento Complex con una case class incapsulata in un object
 * Essendo ComplexWithCaseImpl una case class potrò istanziarla senza new
 * e potrò utilizzare equals e toString direttamente sui class parameters
 * L'istanziazione è nascosta dall'object (simil factory)
 */
object ComplexWithCase{
  def apply(re:Double, im: Double): Complex = ComplexWithCaseImpl(re, im)

  private case class ComplexWithCaseImpl(re: Double, im: Double) extends Complex{

    override def +(c: Complex): Complex = {
      ComplexWithCase(re + c.re, im + c.im)
    }

    override def *(c: Complex): Complex = {
      ComplexWithCase((re * c.re - im * c.im), (re * c.im + im * c.re))
    }
  }
}




object TryComplex extends App {
  val a = Array(new ComplexImpl(10, 20), new ComplexImpl(1, 1), new ComplexImpl(7, 0))
  val c = a(0).+(a(1)) + a(2)
  println("non case class")
  println(c, c.re, c.im) // (u04lab.code.Complex$ComplexImpl@59a6e353,18.0,21.0)
  val c2 = a(0) * a(1)
  println(c2, c2.re, c2.im) // (u04lab.code.Complex$ComplexImpl@96532d6,-10.0,30.0)
  println("Equals: " + new ComplexImpl(1,1).equals(new ComplexImpl(1,1))) //Equals: false

  val b = Array(ComplexWithCase(10, 20), ComplexWithCase(1,1), ComplexWithCase(7,0))
  val d = b(0).+(b(1)) + b(2)
  println("case class")
  println(d, d.re, d.im) // (ComplexImpl(18.0,21.0),18.0,21.0)
  val d2 = b(0) * b(1)
  println(d2, d2.re, d2.im) // (ComplexImpl(-10.0,30.0),-10.0,30.0)
  println("Equals: " + ComplexWithCase(1,1).equals(ComplexWithCase(1,1))) //Equals: true
}

/** Hints:
 * - implement Complex with a ComplexImpl class, similar to PersonImpl in slides
 * - check that equality and toString do not work
 * - use a case class ComplexImpl instead, creating objects without the 'new' keyword
 * - check equality and toString now
 */