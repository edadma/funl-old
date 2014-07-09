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
	"Power_set" in {executeCaptureOutput( "RosettaCode.org/Power_set", Some("-main-") ).trim shouldBe "{{}, {4}, {1, 2}, {1, 3}, {2, 3, 4}, {3}, {1, 2, 3, 4}, {1, 4}, {1, 2, 3}, {2}, {1, 2, 4}, {1}, {3, 4}, {2, 3}, {2, 4}, {1, 3, 4}}"}
	"Pi" in {executeCaptureOutput( "RosettaCode.org/Pi", Some("-main-"), "args" -> Vector("1000") ).trim shouldBe "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446095505822317253594081284811174502841027019385211055596446229489549303819644288109756659334461284756482337867831652712019091456485669234603486104543266482133936072602491412737245870066063155881748815209209628292540917153643678925903600113305305488204665213841469519415116094330572703657595919530921861173819326117931051185480744623799627495673518857527248912279381830119491298336733624406566430860213949463952247371907021798609437027705392171762931767523846748184676694051320005681271452635608277857713427577896091736371787214684409012249534301465495853710507922796892589235420199561121290219608640344181598136297747713099605187072113499999983729780499510597317328160963185950244594553469083026425223082533446850352619311881710100031378387528865875332083814206171776691473035982534904287554687311595628638823537875937519577818577805321712268066130019278766111959092164201989"}
	"Generator/Exponential" in {executeCaptureOutput( "RosettaCode.org/Generator/Exponential", Some("-main-") ).trim shouldBe "[529, 576, 625, 676, 784, 841, 900, 961, 1024, 1089]"}
	"Ackermann_function" in {executeCaptureOutput( "RosettaCode.org/Ackermann_function", Some("-main-") ).trim shouldBe
		"""	|Ackermann( 0, 0 ) = 1
				|Ackermann( 0, 1 ) = 2
				|Ackermann( 0, 2 ) = 3
				|Ackermann( 0, 3 ) = 4
				|Ackermann( 0, 4 ) = 5
				|Ackermann( 1, 0 ) = 2
				|Ackermann( 1, 1 ) = 3
				|Ackermann( 1, 2 ) = 4
				|Ackermann( 1, 3 ) = 5
				|Ackermann( 1, 4 ) = 6
				|Ackermann( 2, 0 ) = 3
				|Ackermann( 2, 1 ) = 5
				|Ackermann( 2, 2 ) = 7
				|Ackermann( 2, 3 ) = 9
				|Ackermann( 2, 4 ) = 11
				|Ackermann( 3, 0 ) = 5
				|Ackermann( 3, 1 ) = 13
				|Ackermann( 3, 2 ) = 29
				|Ackermann( 3, 3 ) = 61
				|Ackermann( 3, 4 ) = 125""".stripMargin}
  "Hamming_numbers" in {executeCaptureOutput( "RosettaCode.org/Hamming_numbers", Some("-main-") ).trim shouldBe
    """ |[1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36]
				|2125764000
				|8100000000""".stripMargin}
  "Matrix_arithmetic" in {executeCaptureOutput( "RosettaCode.org/Matrix_arithmetic", Some("-main-") ).trim shouldBe
    """ |((1, 2), (3, 4)), perm: 10, det: -2
				|((-2, 2, -3), (-1, 1, 3), (2, 0, -1)), perm: 10, det: 18
				|((1, 2, 3, 4), (4, 5, 6, 7), (7, 8, 9, 10), (10, 11, 12, 13)), perm: 29556, det: 0
				|((0, 1, 2, 3, 4), (5, 6, 7, 8, 9), (10, 11, 12, 13, 14), (15, 16, 17, 18, 19), (20, 21, 22, 23, 24)), perm: 6778800, det: 0""".stripMargin}
  "Factors_of_an_integer" in {executeCaptureOutput( "RosettaCode.org/Factors_of_an_integer", Some("-main-") ).trim shouldBe
    """ |The set of factors of 103 is {1, 103}
        |The set of factors of 316 is {158, 4, 79, 1, 2, 316}
        |The set of factors of 519 is {1, 3, 173, 519}
        |The set of factors of 639 is {9, 639, 71, 213, 1, 3}
        |The set of factors of 760 is {8, 19, 4, 40, 152, 5, 10, 76, 1, 95, 190, 760, 20, 2, 38, 380}""".stripMargin}
	"Modular_inverse" in {executeCaptureOutput( "RosettaCode.org/Modular_inverse", Some("-main-") ).trim shouldBe "1969"}
}
