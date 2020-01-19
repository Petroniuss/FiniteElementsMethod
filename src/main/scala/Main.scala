import agh.iet.devs.Functions.RealFunction
import agh.iet.devs.{FiniteElementsMethod, Functions}

object Main extends App {

	val a = Functions.const(1)
	val b: RealFunction = x => x * x
	val c: RealFunction = x => 20 * math.sin(math.Pi * x)
	val f: RealFunction = x => -5 * math.exp(x)
	val beta = 1
	val gamma = 1
	val uR = 2
	val n = 10

	val u = FiniteElementsMethod.solve(a, b, c, f) (beta, gamma, uR, n)
}

