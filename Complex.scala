import scala.math._

/**
  * DEFINITION DU TYPE COMPLEX
  *
  * @param re partie réel
  * @param im partie imaginaire
  */
case class Complex(re: Double, im: Double) extends Ordered[Complex] {
   val modulus = sqrt(pow(re, 2) + pow(im, 2))

  // Constructors
  def this(re: Double) = this(re, 0)

  // Unary operators
  def unary_+ = this

  def unary_- = new Complex(-re, -im)

  def unary_~ = new Complex(re, -im)

  // conjugate
  def unary_! = modulus

  // Comparison
  def compare(that: Complex) = !this compare !that

  // Arithmetic operations
  def +(c: Complex) = new Complex(re + c.re, im + c.im)

  def -(c: Complex) = this + -c

  def *(c: Complex) =
    new Complex(re * c.re - im * c.im, im * c.re + re * c.im)

  def /(c: Complex) = {
    require(c.re != 0 || c.im != 0)
    val d = pow(c.re, 2) + pow(c.im, 2)
    new Complex((re * c.re + im * c.im) / d, (im * c.re - re * c.im) / d)
  }

  // String representation
  override def toString() =
    this match {
      case Complex.i => "i"
      case Complex(re, 0) => re.toString
      case Complex(0, im) => im.toString + "*i"
      case _ => asString
    }

  private def asString =
    re + (if (im < 0) "-" + -im else "+" + im) + "*i"
}

object Complex {
  // Constants
  val i = new Complex(0, 1)

  // Factory methods
  def apply(re: Double) = new Complex(re)

  // Implicit conversions
  implicit def fromDouble(d: Double) = new Complex(d)

  implicit def fromFloat(f: Float) = new Complex(f)

  implicit def fromLong(l: Long) = new Complex(l)

  implicit def fromInt(i: Int) = new Complex(i)

  implicit def fromShort(s: Short) = new Complex(s)

  def afficheListC(list: Array[Complex]) {
    printf("Liste : ( ");
    for (i <- 0 to list.length - 1) {
      printf(list(i) + " _ ");
    }
    println(")");
  }

  def afficheList(list: Array[Double]) {
    printf("Liste : ( ");
    for (i <- 0 to list.length - 1) {
      printf(list(i) + " _ ");
    }
    println(")");
  }

  def doubleToComplex(list: Array[Double]): Array[Complex] = {
    var n: Int = list.length
    var zero: Complex = 0;
    var convert = Array.fill(n)(zero);
    for (j <- 0 to n - 1) {
      convert(j) = new Complex(list(j));
    }
    return convert;
  }


}

