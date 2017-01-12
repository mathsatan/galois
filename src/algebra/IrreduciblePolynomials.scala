package algebra

import algebra.polynomials.Polynomial

import scala.collection.mutable.ListBuffer

object IrreduciblePolynomials{

	def apply(d: Int): List[Int] = {
		if (d < 1 || d > 30)
		throw new IndexOutOfBoundsException("Irreducible polynomial should be in range 1..30")
		calcIrreducible(d)
	}

	private def check(p: Int, list: List[Int]): Boolean = {
		val pol = Polynomial(p)
		list.foreach((item: Int) => {
			val i = Polynomial(item)
			if ((pol div i)._2 == 0){
				return false
			}
		})
		true
	}

	def calcIrreducible(_deg: Int): List[Int] = {
    def calc(deg: Int): List[Int] = {
      if (deg == 1) return List[Int](2, 3)
      // d > 2
      var resultList = ListBuffer[Int]()
      // generate all polynomials of degree d
      val n = 1 << deg
      val nd = if (_deg.equals(deg)) deg >> 1 else deg - 1
      val list: List[Int] = calc(nd)
      for(i <- 0 until n){
        val t = i ^ n		// polynomial of P set, for testing
        if (check(t, list)) resultList += t
      }
      resultList.toList ::: list
    }
    if (_deg < 1) return Nil
    calc(_deg).filter(_ >= (1 << _deg))
	}

}

