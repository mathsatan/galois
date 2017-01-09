package algebra.polynomials

/**
  * Created by max on 11/3/2016.
  *
  * file:///C:/Users/max/Documents/Khorstman_K_-_Scala_dlya_neterpelivykh_-_2013.pdf
  */
/*abstract class AbstractPolynomial {
  protected val polynomial: Int
  def *(other: AbstractPolynomial): AbstractPolynomial
  def /(other: AbstractPolynomial): AbstractPolynomial
}
class BasicPolynomial(p: Int) extends AbstractPolynomial {
  override val polynomial: Int = p

  override def *(other: AbstractPolynomial): AbstractPolynomial = {
    new BasicPolynomial(0)//this. polynomial * other.polynomial
  }

  override def /(other: AbstractPolynomial): AbstractPolynomial = {
    new BasicPolynomial(0)//this. polynomial / other.polynomial
  }
}

trait GF extends AbstractPolynomial {
  val modulo: AbstractPolynomial = new BasicPolynomial(0)
  abstract override def *(other: AbstractPolynomial): AbstractPolynomial = {super.*(other) / modulo}
  abstract override def /(other: AbstractPolynomial): AbstractPolynomial = {super.*(other)}
}
*/

object Polynomial{
  def apply(p: Int) = new Polynomial(p)
}

class Polynomial(_p: Int) {
  val polynomial: Int = _p

  def order: Int = {
    var i: Int = 30
    while(i >= 0){
      if ((polynomial >> i) == 1)
        return i
      i = i - 1
    }
    i
  }

  def mul(p: Polynomial): Polynomial = {
    if (this.order + p.order > 30){
      throw new IndexOutOfBoundsException("Product of polynomials is bigger than integer type")
    }
    if (this.polynomial == 0 || p.polynomial == 0) return Polynomial(0)

    var resultPolynomial: Int = 0
    if ((this.polynomial & 1) == 1){
      resultPolynomial = p.polynomial
    }
    var tempPolynomial: Int = p.polynomial

    for(i <- 1 to this.order){
      tempPolynomial = tempPolynomial << 1

      if ((this.polynomial & (1 << i)) == (1 << i)){  // Math.pow(2, i).toInt == 1 << i
        resultPolynomial = resultPolynomial ^ tempPolynomial
      }
    }

    Polynomial(resultPolynomial)
  }
  def div(p: Polynomial): (Int, Int) = { // if a/b then a = b*q+r, so method returned (q, r)
    var a = this
    val b = p

    if (a.order < b.order){
      return (0, a.polynomial)
    }
    var result: Int = 0
    var c = 0
    var d = 0

    while(a.order >= b.order){
      val t: Int = a.order - b.order
      d = 1 << t
      result ^= d
      c = (Polynomial(d) mul b).polynomial
      a = a add Polynomial(c)
    }
    (result, a.polynomial)
  }
  def add(p: Polynomial): Polynomial = {
    new Polynomial(this.polynomial ^ p.polynomial)
  }
  def sub(p: Polynomial): Polynomial = add(p)

  override def toString: String = Integer.toBinaryString(this.polynomial)
}