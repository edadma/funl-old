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
	"Sorting_algorithms/Heapsort" in {executeCaptureOutput( "RosettaCode.org/Sorting_algorithms/Heapsort", Some("-main-") ).trim shouldBe "ArraySeq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)"}
	"SHA-256" in {executeCaptureOutput( "RosettaCode.org/SHA-256", Some("-main-") ).trim shouldBe
		"""	|FunL: "Rosetta code" ~> 764faf5c61ac315f1497f9dfa542713965b785e5cc2f707d6468d7d1124cdfcf
				|Java: "Rosetta code" ~> 764faf5c61ac315f1497f9dfa542713965b785e5cc2f707d6468d7d1124cdfcf
				|FunL: "" ~> e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
				|Java: "" ~> e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855""".stripMargin}
}
