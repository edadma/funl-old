package funl

import org.scalatest._

import funl.interp.Interpreter._


class Constructs extends FreeSpec with Matchers
{
	"if" in
	{
 		expression( "if true then 3 else 4" ) shouldBe 3
 		expression( "if false then 3" ) shouldBe ()
 		expression( "if false then 3 else 4" ) shouldBe 4
		expression( """
			if true then
				3 """ ) shouldBe 3
		expression( """
			if false then
				3	""" ) shouldBe ()
		expression( """
			if true then
				3
			else
				4	""" ) shouldBe 3
		expression(
			"""	if false then
						3
					else
						4
			""" ) shouldBe 4
		expression(
			"""	if true then
						3
					else 4	""" ) shouldBe 3
		expression(
			"""	if false then
						3
					else 4	""" ) shouldBe 4
		expression(
			"""	if true then 3 else
						4
			""" ) shouldBe 3
		expression(
			"""	if false then 3 else
						4
			""" ) shouldBe 4
	}
}