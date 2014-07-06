package funl

import org.scalatest._
import prop.PropertyChecks

import funl.interp.Interpreter._


class Rosetta extends FreeSpec with PropertyChecks with Matchers
{
	"Return_multiple_values" in {executeCaptureOutput( "RosettaCode.org/Return_multiple_values", Some("-main-") ).trim shouldBe "45, 21, (45, 21)"}
	"Binary_digits" in {executeCaptureOutput( "RosettaCode.org/Binary_digits", Some("-main-") ).trim shouldBe
		"""	|5, 101
				|50, 110010
				|9000, 10001100101000
				|9000000000, 1000011000011100010001101000000000""".stripMargin}
}
