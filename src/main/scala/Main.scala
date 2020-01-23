import agh.iet.devs.FiniteElementsMethod
import agh.iet.devs.Functions.RealFunction

//Example usage
object Main extends App {

	val a: RealFunction = x => (-(x*x)) -1
	val b: RealFunction = x => (4 * x) + 1
	val c: RealFunction = x => -4
	val f: RealFunction = x => (2 * (x*x)) - (4 * x) + 3
	val beta = -0.5
	val gamma = 1
	val uR = 0
	val n = 10

	val u = FiniteElementsMethod.solve(a, b, c, f) (beta, gamma, uR, n)
}

