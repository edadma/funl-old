package funl

import org.scalatest._
import prop.PropertyChecks

import funl.interp.Interpreter._


class Statements extends FreeSpec with PropertyChecks with Matchers
{
	"assignments" in
	{
		snippet( """
			|a = 3
			|b = 4
			|a + b
			""" ) shouldBe Some(7)
	}
}

// def f(a) =
// 	def g(b) =
// 		if b == 0
// 			b
// 		else
// 			g( b - 1 )
//
// 	g(a)
