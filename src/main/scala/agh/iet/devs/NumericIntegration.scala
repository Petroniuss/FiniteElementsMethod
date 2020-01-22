package agh.iet.devs

import Functions._

object NumericIntegration {
	implicit val N: Int = 100

	def ∫(a: Real, b: Real)(f: RealFunction)(implicit n: Int): Double = {
		val deltaX = (b - a) / n
		var approx = 0.0
		var mi = a + deltaX / 2.0

		for (_ <- 0 until n) {
			approx += f(mi) * deltaX
			mi += deltaX
		}

		approx
	}
}
