package funl

import org.scalatest._
import prop.PropertyChecks

import funl.interp.Interpreter._


class Statements extends FreeSpec with PropertyChecks with Matchers
{
	"assignments" in
	{
// 		statement( """
// 			a = 3
// 			b = 4
// 			a + b	""" ) shouldBe 7
	}
}