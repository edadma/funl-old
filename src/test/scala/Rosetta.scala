package funl

import org.scalatest._
import prop.PropertyChecks

import funl.interp.Interpreter._


class Rosetta extends FreeSpec with PropertyChecks with Matchers
{
	"Return_multiple_values" in {executeCaptureOutput( "rosetta/Return_multiple_values", Some("-main-") ).trim shouldBe "45, 21, (45, 21)"}
}
