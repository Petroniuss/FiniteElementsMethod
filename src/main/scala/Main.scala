import agh.iet.devs.Functions
import agh.iet.devs.Functions.{FunctionSyntax, RealFunction}

//Example usage
object Main extends App {

	val a = Functions.const(1)
	val b: RealFunction = x => x * x
	val c: RealFunction = x => 20 * math.sin(math.Pi * x)
	val f: RealFunction = x => -5 * math.exp(x)
	val beta = 1
	val gamma = 1
	val uR = 2
	val n = 250

	val y1: Double => Double = x => 2 * x
	val y2: Double => Double = _ => 1

	val sumY: RealFunction = y1 + y2
}

