package algebra

/**
  * Created by max on 10/11/2016.
  * Trait is an extended GF(2^m)
  **/

trait ExtendedGF {
  def +(p: GFPolynomial): GFPolynomial
  def -(p: GFPolynomial): GFPolynomial
  def *(p: GFPolynomial): GFPolynomial
  def /(p: GFPolynomial): GFPolynomial
}
