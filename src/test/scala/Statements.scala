package funl

import org.scalatest._
import prop.PropertyChecks

import funl.interp.Interpreter._


class Statements extends FreeSpec with PropertyChecks with Matchers
{
	"assignments" in
	{
		snippet( """
a = 3
b = 4
a + b	""" ) shouldBe 7
	}
}