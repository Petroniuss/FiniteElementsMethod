package agh.iet.devs

import agh.iet.devs.Functions._
import agh.iet.devs.NumericIntegration._

object FiniteElementsMethod {
	def solve(a: RealFunction, b: RealFunction, c: RealFunction, f: RealFunction)
	         (beta: Real, gamma: Real, uR: Real, n: Int): RealFunction = {
		new FiniteElementsMethod(a, b, c, f)(beta, gamma, uR, n).solve()
	}

}

class FiniteElementsMethod
	(a: RealFunction, b: RealFunction, c: RealFunction, f: RealFunction)
	(beta: Real, gamma: Real, uR: Real, n: Int) {

	val deltaX: Real = 1.0 / n

	def solve(): RealFunction = {
		val B = new Matrix(n + 1)
		val L = new Vector(n + 1)

		for (i <- 0 to n) {
			L(i) = Li(i)
		}

		println("L\n" + L)

		for (i <- 0 to n;
			 j <- 0 to n) {
			B(i)(j) = Bij(i, j)
		}

		println("B\n" + B)

		val U = LinearEqSolver.thomas(B, L, n + 1)
		var u = Functions.const(0)

		println("U\n" + U)

		for (i <- 0 to n) {
			u = (Functions.const(U(i)) * e(i)) + u
		}

		u
	}

	def Bij(i: Int, j: Int): Real = {
		if (i < n) {
			b(e(j), e(i), eDerivative(j), eDerivative(i))
		} else if (j == n) {
			1.0
		} else {
			0.0
		}
	}

	def Li(i: Int): Real = if (i == n) uR else l(e(i))

	def l(v: RealFunction): Real = {
		NumericIntegration.midpoint(0.0, 1.0, v * f) - gamma * v(0)
	}

	def b(u: RealFunction, v: RealFunction, uDerivative: RealFunction, vDerivative: RealFunction): Real = {
		NumericIntegration.midpoint(0.0, 1.0, v * b * uDerivative) +
		NumericIntegration.midpoint(0.0, 1.0, v * c * u) - (
		NumericIntegration.midpoint(0.0, 1.0, vDerivative * a * uDerivative) + (beta * u(0.0) * v(0.0)))
	}

	def e(i: Int)(x: Double): Real = {
		if (x < 0 || x > 1)
			return 0.0

		if (x >= xi(i - 1) && x <= xi(i)) {
			(x - xi(i - 1)) / deltaX
		} else if (x >= xi(i) && x <= xi(i + 1)) {
			(xi(i + 1) - x) / deltaX
		} else {
			0.0
		}
	}

	def eDerivative(i: Int)(x: Double): Real = {
		if (x < 0 || x > 1)
			return 0.0

		if (x >= xi(i - 1) && x <= xi(i)) {
			1.0 / deltaX
		} else if (x >= xi(i) && x <= xi(i + 1)) {
			-1.0 / deltaX
		} else {
			0.0
		}
	}

	def xi(i: Int): Real = i * deltaX
}
