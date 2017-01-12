import algebra._

object Main {
  def main(args: Array[String]): Unit = {
    val n = 6
    print(IrreduciblePolynomials(n))
    /*val gf: GaloisField = GaloisField(n)
		for(i <- 1 until (1 << n)){
      for(j <- 1 until (1 << n)){
        val a = gf.createPolynomial(i)
        val b = gf.createPolynomial(j)
        val c = (a+b)-b
        if (a != c)
        print("Error")
      }
		}*/
  }
}
