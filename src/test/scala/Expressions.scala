package funl

import org.scalatest._
import prop.PropertyChecks

import funl.interp.Interpreter._


class Expressions extends FreeSpec with PropertyChecks with Matchers
{
	"arithmetic" in
	{
		expression( "3 + 4*5" ) shouldBe 23
		expression( "3 + (4*5)" ) shouldBe 23
		expression( "(3 + 4)*5" ) shouldBe 35
		expression( "a + b", "a" -> 3, "b" -> 4 ) shouldBe 7
	}
	
	"conditional" in
	{
		expression( "5 + (if true then 3 else 4) + 6" ) shouldBe 14
		expression( "5 + (if false then 3 else 4) + 6" ) shouldBe 15
		expression( "5 + (if true then 3) + 6" ) shouldBe 14
		expression( """"5" + (if false then 3) + 6""" ) shouldBe "5()6"
	}
}