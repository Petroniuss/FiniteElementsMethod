package agh.iet.devs

object Functions {
	type Real = Double
	type RealFunction = Double => Double

	val Identity: RealFunction = x => x

	def const(c: Double): RealFunction = _ => c

	implicit class FunctionSyntax(f: RealFunction) {
		def + (g: RealFunction): RealFunction = x => f(x) + g(x)

		def - (g: RealFunction): RealFunction = x => f(x) - g(x)

		def * (g: RealFunction): RealFunction = x => f(x) * g(x)

		def / (g: RealFunction): RealFunction = x => f(x) / g(x)

		def compose (g: RealFunction): RealFunction = x => f(g(x))
	}

}
