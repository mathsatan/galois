package algebra

import algebra.polynomials._

/**
  * Created by max on 10/23/2016.
  * algebra.GaloisField
  * Integer size is 32 bit
  * 0..30 - 31 bit should using (cause 32th bit is sign)
  * all polynomials should be in range 0..29,
  * so irreducible polynomial should has size 1..30
  * Finite field with q=2**m elements
  */

object GaloisField  {
  def apply(_initBitNumber: Int): GaloisField = {
    new GaloisField(_initBitNumber)
  }
}

class GaloisField(_initBitNumber: Int = FiniteField.DEFAULT_BIT_NUMBER) extends FiniteField(_initBitNumber) {
  override val modulo: Polynomial = Polynomial(createModulo().getOrElse(throw new NoSuchElementException("Fail to create irreducible")))

  protected def createModulo(): Option[Int] = {
    val list = IrreduciblePolynomials(this.bits)
    list.headOption
  }

  def createPolynomial(_initPolynomial: Int): GFPolynomial = {
    if ((1 << bits) <= _initPolynomial || _initPolynomial < 0) {
      throw new IndexOutOfBoundsException("Polynomial has wrong order for current field")
    }
    new GFPolynomial(_initPolynomial, bits)
  }
  def createPolynomial(_binInitPolynomial: String): GFPolynomial = {
    try{
      val b = Integer.parseInt(_binInitPolynomial, 2)
      return createPolynomial(b)
      /*val mask = (1 << bits) - 1
      return new GFPolynomial(b & mask, bits)*/
    }catch {
      case e: IndexOutOfBoundsException => print(e.getMessage)
      case e: Exception => print(e.getMessage)
    }
    new GFPolynomial(0, bits)
  }

  object GFPolynomial{
    def apply(_p: Int, _m: Int): GFPolynomial = new GFPolynomial(_p, _m)
  }

  class GFPolynomial private[GaloisField](_p: Int, _m: Int) extends FiniteFieldPolynomial{
    override def equals(other: Any): Boolean = other match {
      case other: GFPolynomial => this.p.polynomial == other.p.polynomial
      case _ => false
    }

    override protected val p = Polynomial(_p)
    /**
      * Copy constructor
      *
      * @param other polynomial for copy
      */
    def this(other: GFPolynomial){
      this(other.p.polynomial, bits)
    }
    override def self: GFPolynomial = this

    override def +(other: GFPolynomial): GFPolynomial = {
      GFPolynomial(this.p.polynomial ^ other.p.polynomial, bits)
    }
    override def -(other: GFPolynomial): GFPolynomial = {  // In this case add and subtract are the same
      this + other
    }

    override def *(other: GFPolynomial): GFPolynomial = {
      val result: Polynomial = this.p mul other.p
      if (result.order >= bits){
        GFPolynomial((result div modulo)._2, bits)
      }
      else GFPolynomial(result.polynomial, bits)
    }

    override def mulInverse: GFPolynomial = {
      if (p.polynomial == 0)
        throw new NoSuchElementException("Error: there is no multiplicative inverse for zero")
      var r1: Polynomial = Polynomial(modulo.polynomial)
      var r2: Polynomial = this.p
      var s1 = Polynomial(1)
      var s2 = Polynomial(0)
      var t1 = Polynomial(0)
      var t2 = Polynomial(1)
      var t = Polynomial(0)
      var s = Polynomial(0)
      var r = Polynomial(0)
      while(r2.polynomial > 0){
        val q: Polynomial = Polynomial((r1 div r2)._1)
        r = r1 sub (q mul r2)
        r1 = r2; r2 = r
        s = s1 sub (q mul s2); s1 = s2; s2 = s
        t = t1 sub (q mul t2); t1 = t2; t2 = t
      }
      GFPolynomial(t1.polynomial, bits)
    }
  }
}