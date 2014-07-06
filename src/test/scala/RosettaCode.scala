package funl

import org.scalatest._
import prop.PropertyChecks

import funl.interp.Interpreter._


class RosettaCode extends FreeSpec with PropertyChecks with Matchers
{
	"Return_multiple_values" in {executeCaptureOutput( "RosettaCode.org/Return_multiple_values", Some("-main-") ).trim shouldBe "45, 21, (45, 21)"}
	"Binary_digits" in {executeCaptureOutput( "RosettaCode.org/Binary_digits", Some("-main-") ).trim shouldBe
		"""	|5, 101
				|50, 110010
				|9000, 10001100101000
				|9000000000, 1000011000011100010001101000000000""".stripMargin}
	"Tree_traversal" in {executeCaptureOutput( "RosettaCode.org/Tree_traversal", Some("-main-") ).trim shouldBe
		"""	|[1, 2, 4, 7, 5, 3, 6, 8, 9]
				|[7, 4, 2, 5, 1, 8, 6, 9, 3]
				|[7, 4, 5, 2, 8, 9, 6, 3, 1]
				|[1, 2, 3, 4, 5, 6, 7, 8, 9]""".stripMargin}
	"Sorting_algorithms/Quicksort" in {executeCaptureOutput( "RosettaCode.org/Sorting_algorithms/Quicksort", Some("-main-") ).trim shouldBe
		"""	|[0, 1, 2, 2, 3, 4]
				|[Daniel, Ethan, Jacob, Juan, Liam, Miguel, William]""".stripMargin}
	"Sorting_algorithms/Merge_sort" in {executeCaptureOutput( "RosettaCode.org/Sorting_algorithms/Merge_sort", Some("-main-") ).trim shouldBe
		"""	|[16, 17, 27, 37, 48, 56, 58, 67, 72, 94]
				|[Alysha, Emily, Emma, Maya, Olivia, Sofía, Sophia]""".stripMargin}
}
