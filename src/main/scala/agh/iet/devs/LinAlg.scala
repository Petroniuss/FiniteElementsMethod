package agh.iet.devs

class Matrix(val n: Int) {
	val xs: Array[Vector] = Array.fill(n){new Vector(n)}

	def apply(i: Int): Vector = xs(i)

	def update(i: Int, v: Vector): Unit = xs(i) = v

	override def toString: String =
		xs.map(vector => vector.toString)
            .reduceLeft((a, b) => a + "\n" + b)
}

class Vector(val n: Int) {
	val xs: Array[Double] = Array.fill(n){0.0}

	def apply(i: Int): Double = xs(i)

	def update(i: Int, v: Double): Unit = xs(i) = v

	override def toString: String =
		"[" + xs.map(v => v.formatted("%1.2f"))
        	    .reduceLeft((a, b) => a  + " " + b) + "]"
}

object Vector {
	implicit class VectorOps(array: Array[Double]) {
		def toVec: Vector = {
			val v = new Vector(array.length)
			for ((x, i) <- array.view.zipWithIndex) v(i) = x
			v
		}
	}
}

object LinearEqSolver {
	def thomas(M: Matrix, H: Vector, n: Int): Vector = {
		val X = new Vector(n)
		val C = new Vector(n)
		val D = new Vector(n)

		sweep(M, H, C, D, n, 0)
		substitution(M, X, C, D, n, n - 1)
	}

	@scala.annotation.tailrec
	private def substitution(M: Matrix, X: Vector, C: Vector, D: Vector, n: Int, i: Int): Vector = {
		if (i < 0)
			return X

		X(i) =  if (i == n - 1) D(i)
				else            D(i) - C(i) * X(i + 1)

		substitution(M, X, C, D, n, i - 1)
	}

	@scala.annotation.tailrec
	private def sweep(M: Matrix, H: Vector, C: Vector, D: Vector, n: Int, i: Int): Unit = {
		if (i >= n)
			return

		i match {
			case 0 =>
				C(0) = M(0)(1) / M(0)(0)
				D(0) = H(0) / M(0)(0)
			case _ =>
				if (i < n - 1)
					C(i) = M(i)(i + 1) / (M(i)(i) - M(i)(i - 1) * C(i - 1))

				D(i) = (H(i) - M(i)(i - 1) * D(i - 1)) / (M(i)(i) - M(i)(i - 1) * C(i - 1))
		}

		sweep(M, H, C, D, n, i + 1)
	}
}