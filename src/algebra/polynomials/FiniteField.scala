package algebra.polynomials

/**
  * Created by max on 12/12/2016.
  */
object FiniteField{
  val MAX_BIT = 29
  val DEFAULT_BIT_NUMBER = 8
}
abstract class FiniteField(_initBitNumber: Int = FiniteField.DEFAULT_BIT_NUMBER) {
  type T = Polynomial
  type GFPolynomial <: FiniteFieldPolynomial

  protected val checkBitNumber: Int => Int = _initBitNumber => if (_initBitNumber > 0 && _initBitNumber < FiniteField.MAX_BIT) _initBitNumber else FiniteField.DEFAULT_BIT_NUMBER
  val modulo: T
  val bits: Int = checkBitNumber(_initBitNumber)

  protected def createModulo(): Option[Int]
  def createPolynomial(_initPolynomial: Int): FiniteFieldPolynomial

  abstract class FiniteFieldPolynomial {

    protected val p: T
    def +(other: GFPolynomial): GFPolynomial
    def -(other: GFPolynomial): GFPolynomial = {
      this + other
    }
    def *(other: GFPolynomial): GFPolynomial
    def /(other: GFPolynomial): GFPolynomial = this * other.mulInverse
    def addInverse: GFPolynomial = self
    def mulInverse: GFPolynomial
    def self: GFPolynomial
    override def toString: String = {
      Integer.toBinaryString(p.polynomial)
    }
  }
}