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

	def calcIrreducible(deg :Int): List[Int] = {
		if (deg == 1) return List[Int](2, 3)
		//if (d == 2) List[Int](3)	// list of irreducible polynomials of degree 2
		// d > 2
		var resultList = ListBuffer[Int]()
		// generate all polynomials of degree d
		val n = 1 << deg
		for(i <- 0 until n){
			val t = i ^ n		// polynomial of P set, for testing
			val list: List[Int] = calcIrreducible(deg >> 1)
			if (check(t, list)) resultList += t
		}
		resultList.toList
	}

/*
	def isIrreducible(p: algebra.Polynomial): Boolean = {
        val t = new Polynomial(2, p.degree)			// 2 = "10" = P(x) = x
        t.module = p.polynomial
        for(i <- 1 to p.degree){
          val s = t mul t
          val r = s div p
          t.polynomial = r._2	// multiple in GF(2)
        }
        if (t.polynomial != 2) return false


		if (!isPrime(p.degree)){
			val list = factorization(p.degree)
			list.foreach((i: Int)=> {
				val x = new Polynomial(2, p.degree)
				val m: Int = p.degree / i
				for(i <- 1 to m){
					val s = x mul x
					val r = s div p
					x.polynomial = r._2	// multiple in GF(2)
				}
				if (gcd(t - x, p) != 1){
					return false
				}
			})

		}
		true  // irreducible
	}

	def gcd(_a: Polynomial, _b: Polynomial): Int = {
		var a = _a
		var b = _b
		var c = new Polynomial(b)
		while (b.polynomial != 0) {
			c = new Polynomial((a div b)._2, a.m)
			a = new Polynomial(b)
			b = c
		}
		Math.abs(a.polynomial)
    }

	def factorization(n: Int): List[Int] = {
		if (n < 2) return Nil
		if (2 to 3 contains n){
			return List[Int](n)
		}
		val m = Math.floor(Math.sqrt(n)).toInt
		var j = m
		while(j > 1){
			if (n%j == 0){
				val q = n/j
				return factorization(q) ::: factorization(j)
			}
			j = j - 1
		}
		List[Int](n)
	}

	def isPrime(x: Int): Boolean = {
		if (x < 2) return false
		if (x == 2) return true
		var i = x - 1
		while(i > 1){
			if (x%i == 0) return false
			i = i - 1
		}
		true
	}*/
}

